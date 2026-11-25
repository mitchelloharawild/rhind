#' Arithmetic with fractions
#'
#' Results are exact, and in lowest terms. Whole numbers are fractions, but
#' results are ordinary doubles if any element of a plain number isn't whole, as
#' these are usually not exact. Use [as_fraction()] to convert them first.
#'
#' @details
#' \describe{
#'   \item{Arithmetic}{`+`, `-`, `*`, `/`, `%%` and `%/%` are exact. `^` is exact if
#'     all exponents are whole numbers, and otherwise gives doubles. Inputs must
#'     be the same size, or size 1.}
#'   \item{Summaries}{`sum()`, `prod()`, `mean()`, `median()`, `min()`, `max()` and
#'     `range()` are exact. `quantile()` gives doubles.}
#'   \item{Cumulative}{`cumsum()`, `cumprod()`, `cummin()` and `cummax()` are exact.}
#'   \item{Rounding}{`round()`, `floor()`, `ceiling()` and `trunc()` are exact.
#'     `round()` gives the even digit for halves. `signif()` gives doubles.}
#'   \item{Other functions}{`abs()` and `sign()` give fractions. `exp()`, `log()`,
#'     `sqrt()` and so on give doubles.}
#' }
#'
#' If a result needs numbers too large for integers, all results are stored as
#' doubles. Results which need numbers larger than 2^53 are missing, with a
#' warning. Use `as.double()` to calculate approximately instead.
#'
#' Logical operators (`&`, `|`, `!`, `any()`, `all()`) are not supported. Base R
#' finds methods for functions like `sum()` with their first argument, so use
#' `sum(x, 1)` instead of `sum(1, x)`.
#'
#' @name fraction-arithmetic
#' @examples
#' fraction(1, 3) + fraction(1, 6)
#' fraction(2, 3) * 2
#' fraction(2, 3)^-2
#'
#' # Not all whole numbers
#' fraction(1, 2) + 0.25
#' fraction(1, 2) + as_fraction(0.25)
#'
#' sum(fraction(1, 1:10))
#' cumsum(fraction(1, c(2, 4, 8)))
NULL

#' @export
#' @method vec_arith fraction
vec_arith.fraction <- function(op, x, y, ...) {
  UseMethod("vec_arith.fraction", y)
}

#' @export
#' @method vec_arith.fraction default
vec_arith.fraction.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.fraction fraction
vec_arith.fraction.fraction <- function(op, x, y, ...) {
  arith_fraction(op, x, y)
}

#' @export
#' @method vec_arith.fraction numeric
vec_arith.fraction.numeric <- function(op, x, y, ...) {
  arith_fraction(op, x, y)
}

#' @export
#' @method vec_arith.fraction logical
vec_arith.fraction.logical <- function(op, x, y, ...) {
  check_all_na(op, x, y, y)
  arith_fraction(op, x, y)
}

#' @export
#' @method vec_arith.fraction MISSING
vec_arith.fraction.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "+" = x,
    "-" = new_fraction(0L - field_n(x), field_d(x)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric fraction
vec_arith.numeric.fraction <- function(op, x, y, ...) {
  arith_fraction(op, x, y)
}

#' @export
#' @method vec_arith.logical fraction
vec_arith.logical.fraction <- function(op, x, y, ...) {
  check_all_na(op, x, y, x)
  arith_fraction(op, x, y)
}

check_all_na <- function(op, x, y, lgl) {
  if (!all(is.na(lgl))) stop_incompatible_op(op, x, y)
}

# A fraction if exact, otherwise a double vector
operand <- function(x, arg) {
  if (is_fraction(x)) return(x)
  x <- as_bare_number(x, arg, call = NULL)
  if (all(is_whole(x))) fraction(x) else x
}

all_whole <- function(x) {
  d <- field_d(frac_simplify(x))
  all(is.na(d) | d == 1)
}

op_codes <- c("+" = 1L, "-" = 2L, "*" = 3L, "/" = 4L, "%%" = 5L, "%/%" = 6L, "^" = 7L)

arith_fraction <- function(op, x, y) {
  if (!op %in% names(op_codes)) stop_incompatible_op(op, x, y)
  x <- operand(x, "x")
  y <- operand(y, "y")
  vec_size_common(x = x, y = y, .call = NULL)
  exact <- is_fraction(x) && is_fraction(y) && (op != "^" || all_whole(y))
  if (!exact) {
    return(get(op, baseenv())(as.double(x), as.double(y)))
  }
  frac_restore(.Call(
    C_frac_arith, op_codes[[op]], field_n(x), field_d(x), field_n(y), field_d(y)
  ))
}
