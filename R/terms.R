#' Rewrite fractions with other denominators
#'
#' `frac_rescale()` rewrites fractions with the denominator `d`, without changing
#' their value. `frac_align()` rewrites all fractions with their least common
#' denominator, the smallest which they can share.
#'
#' Missing fractions, and fractions with a denominator of zero, are kept as they
#' are. Results which need numbers larger than 2^53 are missing, with a warning.
#'
#' @param x A [fraction()] vector.
#' @param d Denominators, as positive whole numbers. Must be the same size as `x`,
#'   or either can be size 1. Results are missing where `d` is missing, and it is
#'   an error if any fraction can't be written exactly with its denominator.
#'
#' @return A [fraction()] vector. As with arithmetic, it is stored as doubles if
#'   `x` or `d` are doubles. It is an error if the least common denominator is
#'   larger than 2^53.
#'
#' @seealso [frac_simplify()] for lowest terms.
#' @examples
#' frac_rescale(fraction(3, 4), 100)
#' frac_rescale(fraction(c(1, 2), 3), c(6, 12))
#'
#' frac_align(fraction(1, c(2, 3)))
#' @export
frac_rescale <- function(x, d) {
  check_fraction(x)
  d <- as_whole(d, "d")
  if (any(d <= 0, na.rm = TRUE)) cli_abort("{.arg d} must be positive.")
  vec_size_common(x = x, d = d)
  rescale(x, d)
}

#' @rdname frac_rescale
#' @export
frac_align <- function(x) {
  check_fraction(x)
  d <- .Call(C_frac_lcd, field_n(x), field_d(x))
  if (is.na(d)) {
    cli_abort("The least common denominator of {.arg x} is larger than 2^53.")
  }
  if (is_integer_stored(x) && d <= .Machine$integer.max) d <- as.integer(d)
  rescale(x, d)
}

#' Approximate with smaller denominators
#'
#' The closest fractions with denominators no larger than `max_d`. Ties give the
#' smaller denominator, and whole numbers which tie (with `max_d = 1`) are rounded
#' towards zero.
#'
#' This is calculated exactly, and considers all denominators up to `max_d`. In
#' comparison, [as_fraction()] gives the first continued fraction convergent of a
#' number which is within a tolerance, so there may be a closer fraction.
#'
#' @param x A [fraction()] vector.
#' @param max_d The largest denominator, as a whole number.
#'
#' @return A [fraction()] vector in lowest terms, stored as integers if `x` is.
#'   Missing values and fractions with a denominator of zero are not approximated.
#' @examples
#' frac_approx(fraction(355, 113), 10)
#'
#' # There are closer fractions than convergents
#' as_fraction(pi, max_d = 100)
#' frac_approx(as_fraction(pi), 100)
#' @export
frac_approx <- function(x, max_d) {
  check_fraction(x)
  check_number(max_d, "max_d", 1, max_whole)
  if (!is_whole(max_d)) cli_abort("{.arg max_d} must be a whole number.")
  frac_restore(.Call(C_frac_approx, field_n(x), field_d(x), max_d))
}

rescale <- function(x, d, call = caller_env()) {
  out <- .Call(C_frac_rescale, field_n(x), field_d(x), d)
  # Location of the first which isn't exact
  if (is.double(out)) {
    at <- function(x) if (length(x) == 1L) x else x[out]
    from <- frac_format(at(x))
    to <- sprintf("%.0f", at(d))
    cli_abort(
      c(
        "{.arg x} must have an exact value with denominator {.arg d}.",
        x = "{from} can't be written with denominator {to} (at location {sprintf('%.0f', out)})."
      ),
      call = call
    )
  }
  frac_restore(out)
}

#' Pool fractions
#'
#' The sum of the numerators over the sum of the denominators, which pools
#' proportions such as 3 of 10 and 30 of 100 into 33 of 110.
#'
#' Unlike `mean()`, which weights all fractions equally, `frac_pool()` weights
#' fractions by their denominators. So it depends on how fractions are written:
#' pooling `1/2` with `0/1` is `1/3`, but pooling `2/4` with `0/1` is `2/5`.
#' Calculations reduce fractions to lowest terms, so pool the original fractions.
#'
#' `0/0` contributes nothing to the total, and `1/0` or `-1/0` give the same result
#' (or `0/0` if there are both). Totals larger than 2^53 are missing, with a warning.
#'
#' @param x A [fraction()] vector.
#' @param na.rm Should missing values be removed?
#'
#' @return A [fraction()] of size 1, which isn't reduced to lowest terms. It is
#'   `0/0` if `x` is empty, and missing if any elements of `x` are missing.
#' @examples
#' x <- fraction(c(3, 30, 1), c(10, 100, 2))
#' frac_pool(x)
#' mean(x)
#' @export
frac_pool <- function(x, na.rm = FALSE) {
  check_fraction(x)
  # 0/0 is also removed, which doesn't change the result
  x <- drop_missing(x, na.rm)
  frac_restore(.Call(C_frac_pool, field_n(x), field_d(x)))
}
