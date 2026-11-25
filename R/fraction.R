# Doubles have 53 significant bits (.Machine$double.digits), so whole numbers up
# to this size are exact. Must match MAX_WHOLE in src/rhind.c.
max_whole <- 2^53

#' Exact fractions
#'
#' A `fraction` vector stores the numerator and denominator of each element
#' separately, as whole numbers. Fractions are kept as they were written, so
#' `fraction(2, 4)` is `2/4`, while results of [calculations][fraction-arithmetic]
#' are exact and in lowest terms.
#'
#' The numerator and denominator are either both integers, or both doubles if
#' either input is a double. Integers use half the memory, and become doubles if
#' a result needs larger numbers. Doubles are limited to whole numbers up to 2^53
#' (about 9e15).
#'
#' The sign is stored in the numerator. A denominator of zero gives `1/0`, `-1/0`
#' and `0/0`, which behave as `Inf`, `-Inf` and `NaN`. Names and dimensions are not
#' supported; they are dropped and an error respectively.
#'
#' @param n,d For `fraction()` and `new_fraction()`, the numerator and
#'   denominator as whole numbers. `fraction()` recycles inputs of size 1. If
#'   either is missing, the fraction is missing.
#' @param x An object to test.
#'
#' @return `fraction()` and `new_fraction()` return a `fraction` vector.
#'   `is_fraction()` returns `TRUE` or `FALSE`.
#'
#' @seealso [fraction-arithmetic], [fraction-comparison], [as_fraction()],
#'   [numerator()]
#' @examples
#' fraction(1, 3) + fraction(1, 6)
#'
#' # The fraction is kept as written
#' x <- fraction(c(2, 30), c(4, 100))
#' x
#' simplify(x)
#'
#' # Stored as integers
#' fraction(1:3, 4L)
#' @export
fraction <- function(n = integer(), d = 1L) {
  n <- as_whole(n, "n")
  d <- as_whole(d, "d")
  size <- vec_size_common(n = n, d = d)
  if (is.integer(n) != is.integer(d)) {
    n <- as.double(n)
    d <- as.double(d)
  }
  n <- rep_len(n, size)
  d <- rep_len(d, size)

  missing <- is.na(n) | is.na(d)
  if (any(missing)) n[missing] <- d[missing] <- NA
  negative <- which(d < 0L)
  n[negative] <- 0L - n[negative]
  d[negative] <- -d[negative]
  new_fraction(n, d)
}

#' @description `new_fraction()` is a low-level constructor for developers. It
#'   requires `n` and `d` to be of the same type (integer or double) and size, with
#'   non-negative `d`, and doesn't check their values.
#' @rdname fraction
#' @export
new_fraction <- function(n = integer(), d = integer()) {
  for (x in list(n, d)) {
    if (!(is.integer(x) || is.double(x)) || !is.null(attributes(x))) {
      cli_abort("{.arg n} and {.arg d} must be numbers without attributes.")
    }
  }
  if (typeof(n) != typeof(d)) {
    cli_abort("{.arg n} and {.arg d} must be the same type.")
  }
  if (length(n) != length(d)) {
    cli_abort("{.arg n} and {.arg d} must be the same size.")
  }
  new_rcrd(list(n = n, d = d), class = "fraction")
}

#' @rdname fraction
#' @export
is_fraction <- function(x) {
  inherits(x, "fraction")
}

# Remove all attributes, rejecting dimensions (since size would be ambiguous)
as_bare_number <- function(x, arg = "x", call = caller_env()) {
  if (!is.null(dim(x))) {
    cli_abort("{.arg {arg}} must not have dimensions.", call = call)
  }
  if (!(is.integer(x) && !is.object(x))) {
    to <- if (is.logical(x) && all(is.na(x))) integer() else double()
    x <- vec_cast(x, to, x_arg = arg, call = call)
  }
  if (!is.null(attributes(x))) attributes(x) <- NULL
  x
}

# Element which can be stored exactly
is_whole <- function(x) {
  if (is.integer(x)) return(rep_len(TRUE, length(x)))
  is.na(x) & !is.nan(x) | is.finite(x) & abs(x) <= max_whole & x == trunc(x)
}

as_whole <- function(x, arg, call = caller_env()) {
  # NaN has no fraction, but is missing
  x <- as_bare_number(x, arg, call)
  if (is.double(x)) {
    x[is.nan(x)] <- NA
    if (!all(is_whole(x))) {
      cli_abort(
        c(
          "{.arg {arg}} must be whole numbers no larger than 2^53.",
          i = "Use {.fn as_fraction} to approximate other numbers."
        ),
        call = call
      )
    }
    # No negative zero
    x <- x + 0
  }
  x
}

check_fraction <- function(x, arg = "x", call = caller_env()) {
  if (!is_fraction(x)) {
    cli_abort(
      "{.arg {arg}} must be a {.cls fraction} vector, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}

field_n <- function(x) .subset2(x, "n")
field_d <- function(x) .subset2(x, "d")
frac_value <- function(x) {
  out <- field_n(x) / field_d(x)
  out[is.na(field_n(x))] <- NA_real_
  out
}

# From a list of fields
restore <- function(x) new_fraction(x$n, x$d)

#' Components of fractions
#'
#' @param x A [fraction()] vector.
#'
#' @return
#' * `numerator()` and `denominator()`: integer or double vectors, as stored.
#' * `simplify()`: `x` in lowest terms.
#'
#' @examples
#' x <- fraction(c(2, 30), c(4, 100))
#' numerator(x)
#' denominator(x)
#' simplify(x)
#' @export
numerator <- function(x) {
  check_fraction(x)
  field_n(x)
}

#' @rdname numerator
#' @export
denominator <- function(x) {
  check_fraction(x)
  field_d(x)
}

#' @rdname numerator
#' @export
simplify <- function(x) {
  check_fraction(x)
  restore(.Call(C_frac_reduce, field_n(x), field_d(x)))
}

# vctrs proxies ----------------------------------------------------------------

# Equal values are equal in lowest terms
#' @export
vec_proxy_equal.fraction <- function(x, ...) {
  x <- simplify(x)
  n <- as.double(field_n(x))
  d <- as.double(field_d(x))
  nan <- which(n == 0 & d == 0)
  n[nan] <- d[nan] <- NaN
  new_data_frame(list(n = n, d = d))
}

#' @export
vec_proxy_compare.fraction <- function(x, ...) {
  new_data_frame(.Call(C_frac_order, field_n(x), field_d(x)))
}

#' @export
vec_proxy_order.fraction <- function(x, ...) {
  vec_proxy_compare.fraction(x)
}

# Matches numbers which are exactly the value of the fraction
#' @exportS3Method NULL
mtfrm.fraction <- function(x) {
  x <- simplify(x)
  out <- frac_format(x)
  exact <- which(field_d(x) == 0 | log2(field_d(x)) %% 1 == 0)
  out[exact] <- as.character(frac_value(x)[exact])
  out
}

# Missing values ---------------------------------------------------------------

#' @export
is.na.fraction <- function(x) {
  is.na(frac_value(x))
}

#' @export
anyNA.fraction <- function(x, recursive = FALSE) {
  anyNA(frac_value(x))
}

# Like base vectors, assigning past the end extends with missing values. Needed
# by rbind() for data frames.
#' @export
`[<-.fraction` <- function(x, i, value) {
  if (missing(i)) i <- TRUE
  if (is.numeric(i) && !is.object(i) && length(i)) {
    size <- max(i, 0, na.rm = TRUE)
    if (size > length(x)) length(x) <- size
  }
  value <- vec_cast(value, x)
  vec_assign(x, i, value)
}
