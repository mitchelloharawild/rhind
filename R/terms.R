#' Rewrite fractions with other denominators
#'
#' `frac_rescale()` rewrites fractions with the denominator `d`, without changing
#' their value.
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
#'   `x` or `d` are doubles.
#'
#' @seealso [frac_simplify()] for lowest terms.
#' @examples
#' frac_rescale(fraction(3, 4), 100)
#' frac_rescale(fraction(c(1, 2), 3), c(6, 12))
#' @export
frac_rescale <- function(x, d) {
  check_fraction(x)
  d <- as_whole(d, "d")
  if (any(d <= 0, na.rm = TRUE)) cli_abort("{.arg d} must be positive.")
  vec_size_common(x = x, d = d)
  rescale(x, d)
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
