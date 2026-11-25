#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>

__extension__ typedef __int128 i128;
__extension__ typedef unsigned __int128 u128;

#define MAX_WHOLE 9007199254740992.0 /* 2^53 */
#define I128_MIN (-(i128) (~(u128) 0 >> 1) - 1)

/* Reading ------------------------------------------------------------------ */

typedef struct {
  const int *in, *id;
  const double *dn, *dd;
  R_xlen_t len;
} fracs;

static fracs read_fracs(SEXP n, SEXP d) {
  fracs f = {NULL, NULL, NULL, NULL, XLENGTH(n)};
  if (TYPEOF(n) != TYPEOF(d) || XLENGTH(d) != f.len)
    Rf_error("Internal error: fields must have the same type and size.");
  if (TYPEOF(n) == INTSXP) {
    f.in = INTEGER_RO(n);
    f.id = INTEGER_RO(d);
  } else if (TYPEOF(n) == REALSXP) {
    f.dn = REAL_RO(n);
    f.dd = REAL_RO(d);
  } else {
    Rf_error("Internal error: fields must be integer or double.");
  }
  return f;
}

/* Wide enough for running totals */
typedef struct {
  i128 n, d;
  int na;
} frac;

static const frac FRAC_NA = {0, 0, 1};

static inline frac get(const fracs *f, R_xlen_t i) {
  frac out = FRAC_NA;
  if (f->in) {
    if (f->in[i] != NA_INTEGER && f->id[i] != NA_INTEGER) {
      out.n = f->in[i];
      out.d = f->id[i];
      out.na = 0;
    }
  } else if (!ISNAN(f->dn[i]) && !ISNAN(f->dd[i])) {
    out.n = (int64_t) f->dn[i];
    out.d = (int64_t) f->dd[i];
    out.na = 0;
  }
  return out;
}

/* Arithmetic --------------------------------------------------------------- */

static u128 gcd(u128 a, u128 b) {
  while (b) {
    if (a <= UINT64_MAX && b <= UINT64_MAX) {
      uint64_t x = (uint64_t) a, y = (uint64_t) b;
      while (y) {
        uint64_t t = x % y;
        x = y;
        y = t;
      }
      return x;
    }
    u128 t = a % b;
    a = b;
    b = t;
  }
  return a;
}

/* Lowest terms, with the sign in the numerator. n/0 becomes the sign of n. */
static frac make(i128 n, i128 d) {
  if (d < 0) {
    n = -n;
    d = -d;
  }
  frac out = {n, d, 0};
  u128 g = gcd((u128) (n < 0 ? -n : n), (u128) d);
  if (g > 1) {
    out.n = n / (i128) g;
    out.d = d / (i128) g;
  }
  return out;
}

/* Operations with infinite fractions only give special values and zero */
static frac from_special(double x) {
  if (ISNAN(x)) return make(0, 0);
  if (!R_FINITE(x)) return make(x > 0 ? 1 : -1, 0);
  return make((i128) x, 1);
}

static double to_double(frac x) {
  if (x.na) return NA_REAL;
  if (x.d == 0) return x.n > 0 ? R_PosInf : x.n < 0 ? R_NegInf : R_NaN;
  return (double) x.n / (double) x.d;
}

static i128 floor_div(i128 a, i128 b) {
  i128 q = a / b;
  if (a % b != 0 && ((a < 0) != (b < 0))) q--;
  return q;
}

enum { OP_ADD = 1, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_IDIV, OP_POW };

static int too_large(i128 x) {
  return x > (i128) 1 << 53 || x < -((i128) 1 << 53);
}

/* y is whole, *overflow is set if not representable */
static frac power(frac x, frac y, int *overflow) {
  if (!y.na && y.n == 0 && y.d != 0) return make(1, 1);
  if (!x.na && x.n == x.d && x.d != 0) return make(1, 1);
  if (x.na || y.na) return FRAC_NA;
  if (x.d == 0) return from_special(pow(to_double(x), to_double(y)));
  x = make(x.n, x.d);
  i128 k = y.n / y.d;
  if (k < 0) {
    x = make(x.d, x.n); /* 0/1 becomes 1/0 */
    k = -k;
    if (x.d == 0) return x;
  }
  if (x.n >= -1 && x.n <= 1 && x.d == 1) {
    return make(x.n < 0 && k % 2 == 0 ? 1 : x.n, 1);
  }
  if (k > 53) {
    *overflow = 1;
    return FRAC_NA;
  }
  frac out = make(1, 1);
  for (int i = 0; i < (int) k; i++) {
    out.n *= x.n;
    out.d *= x.d;
    if (too_large(out.n) || too_large(out.d)) {
      *overflow = 1;
      return FRAC_NA;
    }
  }
  return out;
}

/* Operands are stored values (<= 2^53), so products don't overflow */
static frac arith(int op, frac x, frac y, int *overflow) {
  if (op == OP_POW) return power(x, y, overflow);
  if (x.na || y.na) return FRAC_NA;

  if (x.d == 0 || y.d == 0) {
    double a = to_double(x), b = to_double(y);
    switch (op) {
    case OP_ADD: return from_special(a + b);
    case OP_SUB: return from_special(a - b);
    case OP_MUL: return from_special(a * b);
    case OP_DIV:
      if (y.n == 0 && y.d != 0) return make(x.n, 0); /* Inf / 0 */
      return from_special(a / b);
    case OP_MOD:
      if (x.d == 0 || ISNAN(b)) return make(0, 0);
      if (x.n == 0 || (x.n > 0) == (y.n > 0)) return make(x.n, x.d);
      return y;
    default:
      if (x.d == 0) return from_special(y.n == 0 && y.d != 0 ? a / 0.0 : a / b);
      if (ISNAN(b)) return make(0, 0);
      return make(x.n == 0 || (x.n > 0) == (y.n > 0) ? 0 : -1, 1);
    }
  }

  i128 a = x.n * y.d, b = y.n * x.d;
  switch (op) {
  case OP_ADD: return make(a + b, x.d * y.d);
  case OP_SUB: return make(a - b, x.d * y.d);
  case OP_MUL: return make(x.n * y.n, x.d * y.d);
  case OP_DIV:
    if (b == 0) return make(a, 0);
    return make(a, b);
  case OP_MOD:
    if (b == 0) return make(0, 0);
    return make(a - floor_div(a, b) * b, x.d * y.d);
  default:
    if (b == 0) return make(a, 0);
    return make(floor_div(a, b), 1);
  }
}

/* Writing ------------------------------------------------------------------ */

typedef struct {
  SEXP n, d;
  double *pn, *pd;
  int fits;     /* in integers */
  int overflow; /* of doubles */
} writer;

/* Leaves 2 objects protected */
static writer new_writer(R_xlen_t len) {
  writer w;
  w.n = PROTECT(Rf_allocVector(REALSXP, len));
  w.d = PROTECT(Rf_allocVector(REALSXP, len));
  w.pn = REAL(w.n);
  w.pd = REAL(w.d);
  w.fits = 1;
  w.overflow = 0;
  return w;
}

static inline void put(writer *w, R_xlen_t i, frac x) {
  if (!x.na && (too_large(x.n) || too_large(x.d))) {
    w->overflow = 1;
    x.na = 1;
  }
  if (x.na) {
    w->pn[i] = w->pd[i] = NA_REAL;
    return;
  }
  if (x.n > INT_MAX || x.n < -INT_MAX || x.d > INT_MAX) w->fits = 0;
  w->pn[i] = (double) x.n;
  w->pd[i] = (double) x.d;
}

static SEXP finish(writer *w, int integer) {
  const char *names[] = {"n", "d", ""};
  SEXP out = PROTECT(Rf_mkNamed(VECSXP, names));
  if (integer && w->fits) {
    SET_VECTOR_ELT(out, 0, Rf_coerceVector(w->n, INTSXP));
    SET_VECTOR_ELT(out, 1, Rf_coerceVector(w->d, INTSXP));
  } else {
    SET_VECTOR_ELT(out, 0, w->n);
    SET_VECTOR_ELT(out, 1, w->d);
  }
  if (w->overflow)
    Rf_warningcall(R_NilValue, "NAs produced by fractions too large to store exactly");
  UNPROTECT(3);
  return out;
}

/* Kernels ------------------------------------------------------------------ */

#define CHECK_INTERRUPT(i) if (((i) & 0xFFFFF) == 0xFFFFF) R_CheckUserInterrupt()

static R_xlen_t recycled(const fracs *x, const fracs *y) {
  if (x->len == y->len || y->len == 1) return x->len;
  if (x->len != 1) Rf_error("Internal error: incompatible sizes.");
  return y->len;
}

SEXP frac_arith(SEXP op_, SEXP n1, SEXP d1, SEXP n2, SEXP d2) {
  int op = Rf_asInteger(op_);
  fracs x = read_fracs(n1, d1), y = read_fracs(n2, d2);
  R_xlen_t len = recycled(&x, &y);
  writer w = new_writer(len);
  for (R_xlen_t i = 0; i < len; i++) {
    CHECK_INTERRUPT(i);
    put(&w, i, arith(op, get(&x, x.len == 1 ? 0 : i),
                     get(&y, y.len == 1 ? 0 : i), &w.overflow));
  }
  return finish(&w, x.in && y.in);
}

/* -1, 0, 1 or NA_INTEGER */
static int compare(frac x, frac y) {
  if (x.na || y.na) return NA_INTEGER;
  if (x.d == 0 || y.d == 0) {
    double a = to_double(x), b = to_double(y);
    if (ISNAN(a) || ISNAN(b)) return NA_INTEGER;
    return (a > b) - (a < b);
  }
  i128 a = x.n * y.d, b = y.n * x.d;
  return (a > b) - (a < b);
}

SEXP frac_compare(SEXP n1, SEXP d1, SEXP n2, SEXP d2) {
  fracs x = read_fracs(n1, d1), y = read_fracs(n2, d2);
  R_xlen_t len = recycled(&x, &y);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, len));
  int *p = INTEGER(out);
  for (R_xlen_t i = 0; i < len; i++) {
    CHECK_INTERRUPT(i);
    p[i] = compare(get(&x, x.len == 1 ? 0 : i), get(&y, y.len == 1 ? 0 : i));
  }
  UNPROTECT(1);
  return out;
}

SEXP frac_reduce(SEXP n, SEXP d) {
  fracs x = read_fracs(n, d);
  writer w = new_writer(x.len);
  for (R_xlen_t i = 0; i < x.len; i++) {
    CHECK_INTERRUPT(i);
    frac v = get(&x, i);
    put(&w, i, v.na ? v : make(v.n, v.d));
  }
  return finish(&w, x.in != NULL);
}

/* One step of a sum or product with a wide running total */
static frac accumulate(int op, frac s, frac v, int *overflow) {
  if (s.na || v.na) return FRAC_NA;
  if (s.d == 0 || v.d == 0) {
    double a = to_double(s), b = to_double(v);
    return from_special(op == OP_ADD ? a + b : a * b);
  }
  i128 n, d, a, b;
  int bad;
  if (op == OP_ADD) {
    i128 g = (i128) gcd((u128) s.d, (u128) v.d);
    i128 sd = s.d / g, vd = v.d / g;
    bad = __builtin_mul_overflow(s.n, vd, &a) |
      __builtin_mul_overflow(v.n, sd, &b) |
      __builtin_mul_overflow(s.d, vd, &d);
    bad = bad || __builtin_add_overflow(a, b, &n);
  } else {
    frac p = make(s.n, v.d), q = make(v.n, s.d);
    bad = __builtin_mul_overflow(p.n, q.n, &n) |
      __builtin_mul_overflow(p.d, q.d, &d);
  }
  /* Most negative number can't be negated */
  if (bad || n == I128_MIN) {
    *overflow = 1;
    return FRAC_NA;
  }
  return make(n, d);
}

/* Cumulative, or a single value divided by `by` */
SEXP frac_accumulate(SEXP op_, SEXP n, SEXP d, SEXP cumulative_, SEXP by_) {
  int op = Rf_asInteger(op_), cumulative = Rf_asLogical(cumulative_);
  fracs x = read_fracs(n, d);
  writer w = new_writer(cumulative ? x.len : 1);
  frac s = op == OP_ADD ? make(0, 1) : make(1, 1);
  /* A total which is too large stays NA, as do missing values */
  for (R_xlen_t i = 0; i < x.len; i++) {
    CHECK_INTERRUPT(i);
    s = accumulate(op, s, get(&x, i), &w.overflow);
    if (cumulative) put(&w, i, s);
  }
  if (!cumulative) {
    double by = Rf_asReal(by_);
    if (by != 1 && !s.na) {
      if (by == 0) s = make(0, 0);
      else {
        frac inv = {1, (i128) by, 0};
        s = accumulate(OP_MUL, s, inv, &w.overflow);
      }
    }
    put(&w, 0, s);
  }
  return finish(&w, x.in != NULL);
}

/* Positions (from 1) of the smallest (sign = 1) or largest (sign = -1)
 * elements so far. Missing values (or NaN) are selected once seen. */
SEXP frac_extreme(SEXP n, SEXP d, SEXP sign_, SEXP cumulative_) {
  int sign = Rf_asInteger(sign_), cumulative = Rf_asLogical(cumulative_);
  fracs x = read_fracs(n, d);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, cumulative ? x.len : x.len > 0));
  double *p = REAL(out);
  R_xlen_t best = 0;
  for (R_xlen_t i = 0; i < x.len; i++) {
    CHECK_INTERRUPT(i);
    frac b = get(&x, best), v = get(&x, i);
    int b_nan = b.na || (b.n == 0 && b.d == 0);
    int v_nan = v.na || (v.n == 0 && v.d == 0);
    if (b_nan) {
      if (v.na && !b.na && !cumulative) best = i; /* NA over NaN */
    } else if (v_nan || sign * compare(v, b) < 0) {
      best = i;
    }
    if (cumulative) p[i] = (double) (best + 1);
  }
  if (!cumulative && x.len) p[0] = (double) (best + 1);
  UNPROTECT(1);
  return out;
}

enum { R_FLOOR = 1, R_CEILING, R_TRUNC, R_HALF_EVEN };

/* Round to multiples of 10^-digits, for digits between -17 and 15 */
SEXP frac_round(SEXP n, SEXP d, SEXP mode_, SEXP digits_) {
  int mode = Rf_asInteger(mode_);
  fracs x = read_fracs(n, d);
  const int *digits = INTEGER_RO(digits_);
  R_xlen_t nd = XLENGTH(digits_);
  writer w = new_writer(x.len);
  for (R_xlen_t i = 0; i < x.len; i++) {
    CHECK_INTERRUPT(i);
    frac v = get(&x, i);
    if (v.na || v.d == 0) {
      put(&w, i, v.na ? v : make(v.n, 0));
      continue;
    }
    int k = digits[nd == 1 ? 0 : i];
    i128 ten = 1;
    for (int j = 0; j < (k < 0 ? -k : k); j++) ten *= 10;
    i128 a = v.n, b = v.d;
    if (k > 0) a *= ten; else b *= ten;
    i128 q = floor_div(a, b), r = a - q * b;
    switch (mode) {
    case R_CEILING: q += r > 0; break;
    case R_TRUNC: q += r > 0 && a < 0; break;
    case R_HALF_EVEN:
      if (r > b - r || (r == b - r && q % 2 != 0)) q++;
      break;
    }
    put(&w, i, k > 0 ? make(q, ten) : make(q * ten, 1));
  }
  return finish(&w, x.in != NULL);
}

/* The rounded value and its remainder, which order exactly */
SEXP frac_order(SEXP n, SEXP d) {
  fracs x = read_fracs(n, d);
  const char *names[] = {"q", "r", ""};
  SEXP out = PROTECT(Rf_mkNamed(VECSXP, names));
  SEXP q = Rf_allocVector(REALSXP, x.len);
  SET_VECTOR_ELT(out, 0, q);
  SEXP r = Rf_allocVector(REALSXP, x.len);
  SET_VECTOR_ELT(out, 1, r);
  double *pq = REAL(q), *pr = REAL(r);
  for (R_xlen_t i = 0; i < x.len; i++) {
    CHECK_INTERRUPT(i);
    frac v = get(&x, i);
    double value = to_double(v);
    pq[i] = value;
    if (ISNAN(value)) pr[i] = value;
    else if (v.d == 0) pr[i] = 0;
    else pr[i] = fma(-value, (double) v.d, (double) v.n) / (double) v.d;
  }
  UNPROTECT(1);
  return out;
}

/* The first continued fraction convergent within a relative tolerance */
SEXP frac_approx(SEXP x_, SEXP tol_, SEXP max_d_) {
  const double *x = REAL_RO(x_);
  double tol = Rf_asReal(tol_), max_d = Rf_asReal(max_d_);
  R_xlen_t len = XLENGTH(x_);
  writer w = new_writer(len);
  for (R_xlen_t i = 0; i < len; i++) {
    CHECK_INTERRUPT(i);
    if (ISNAN(x[i]) || !R_FINITE(x[i])) {
      put(&w, i, R_IsNA(x[i]) ? FRAC_NA : from_special(x[i]));
      continue;
    }
    double v = fabs(x[i]), rest = v;
    if (v > MAX_WHOLE) {
      w.overflow = 1;
      put(&w, i, FRAC_NA);
      continue;
    }
    double n0 = 0, d0 = 1, n1 = 1, d1 = 0;
    for (int j = 0; j < 64; j++) {
      double a = floor(rest);
      double n2 = a * n1 + n0, d2 = a * d1 + d0;
      if (j > 0 && (d2 > max_d || n2 > MAX_WHOLE)) break;
      n0 = n1; d0 = d1; n1 = n2; d1 = d2;
      if (fabs(v - n1 / d1) <= tol * v || rest == a) break;
      rest = 1 / (rest - a);
    }
    put(&w, i, make((i128) (x[i] < 0 ? -n1 : n1), (i128) d1));
  }
  return finish(&w, 0);
}

/* Missing values are NA, and d = 1 is omitted */
SEXP frac_format(SEXP n, SEXP d) {
  fracs x = read_fracs(n, d);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, x.len));
  char buf[64];
  for (R_xlen_t i = 0; i < x.len; i++) {
    CHECK_INTERRUPT(i);
    frac v = get(&x, i);
    if (v.na) {
      SET_STRING_ELT(out, i, NA_STRING);
      continue;
    }
    if (v.d == 1) snprintf(buf, sizeof buf, "%lld", (long long) v.n);
    else snprintf(buf, sizeof buf, "%lld/%lld", (long long) v.n, (long long) v.d);
    SET_STRING_ELT(out, i, Rf_mkChar(buf));
  }
  UNPROTECT(1);
  return out;
}

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef call_entries[] = {
  CALLDEF(frac_arith, 5),
  CALLDEF(frac_compare, 4),
  CALLDEF(frac_reduce, 2),
  CALLDEF(frac_accumulate, 5),
  CALLDEF(frac_extreme, 4),
  CALLDEF(frac_round, 4),
  CALLDEF(frac_order, 2),
  CALLDEF(frac_approx, 3),
  CALLDEF(frac_format, 2),
  {NULL, NULL, 0}
};

void R_init_rhind(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
