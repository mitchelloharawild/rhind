#' Comparing fractions
#'
#' Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`) are of the fraction's value, so
#' `2/4` equals `1/2`. Comparisons of fractions and whole numbers are exact. Values
#' are compared as doubles if any element of a plain number isn't whole.
#'
#' [unique()], [duplicated()], [match()], [sort()], [order()] and grouping also
#' use the exact value. The first of equal fractions is kept as it was written.
#'
#' @name fraction-comparison
#' @examples
#' fraction(2, 4) == fraction(1, 2)
#' fraction(1, 3) < 0.5
#' unique(fraction(c(2, 1, 1), c(4, 2, 3)))
#' sort(fraction(c(2, 1, 5), c(3, 2, 8)))
NULL

compare_fraction <- function(op, e1, e2) {
  for (e in list(e1, e2)) {
    ok <- is_fraction(e) ||
      (is.numeric(e) && !is.object(e)) ||
      (is.logical(e) && !is.object(e) && all(is.na(e)))
    if (!ok) stop_incompatible_op(op, e1, e2)
  }
  x <- operand(e1, "e1")
  y <- operand(e2, "e2")
  vec_size_common(x = x, y = y, .call = NULL)
  fn <- get(op, baseenv())
  if (!is_fraction(x) || !is_fraction(y)) {
    return(fn(as.double(x), as.double(y)))
  }
  fn(.Call(C_frac_compare, field_n(x), field_d(x), field_n(y), field_d(y)), 0L)
}

#' @export
`==.fraction` <- function(e1, e2) compare_fraction("==", e1, e2)

#' @export
`!=.fraction` <- function(e1, e2) compare_fraction("!=", e1, e2)

#' @export
`<.fraction` <- function(e1, e2) compare_fraction("<", e1, e2)

#' @export
`<=.fraction` <- function(e1, e2) compare_fraction("<=", e1, e2)

#' @export
`>.fraction` <- function(e1, e2) compare_fraction(">", e1, e2)

#' @export
`>=.fraction` <- function(e1, e2) compare_fraction(">=", e1, e2)
