#' Format fractions
#'
#' Fractions are displayed as they are stored, without reducing to lowest terms.
#' Denominators of 1 are omitted.
#'
#' @param x A [fraction()] vector.
#' @param ... Not used.
#'
#' @return An unnamed character vector the size of `x`. Missing values are
#'   `"NA"`, or `NA` with `as.character()`.
#' @examples
#' format(fraction(c(1, 2, 3, NA), c(2, 4, 1, 1)))
#' @export
format.fraction <- function(x, ...) {
  out <- frac_format(x)
  out[is.na(out)] <- "NA"
  out
}

# Missing values are NA
frac_format <- function(x) {
  .Call(C_frac_format, field_n(x), field_d(x))
}

#' @export
vec_ptype_abbr.fraction <- function(x, ...) {
  "frac"
}

#' @export
vec_ptype_full.fraction <- function(x, ...) {
  paste0("fraction<", typeof(field_n(x)), ">")
}

#' @export
obj_print_data.fraction <- function(x, ..., max = getOption("max.print")) {
  n <- length(x)
  if (n == 0L) return(invisible(x))
  shown <- if (n > max) x[seq_len(max)] else x
  print(format(shown), quote = FALSE, right = TRUE, max = max)
  if (n > max) {
    cat(" [ omitted ", n - max, " entries ]\n", sep = "")
  }
  invisible(x)
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.fraction <- function(x, ...) {
  pillar::new_pillar_shaft_simple(frac_format(x), align = "right")
}
