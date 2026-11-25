#' Convert to fractions
#'
#' Whole numbers are converted exactly. Other numbers are approximated with the
#' first continued fraction convergent within a tolerance, since numbers such as
#' 0.1 are not stored exactly by computers.
#'
#' @param x A numeric, character or [fraction()] vector. Text can be a whole
#'   number (`"3"`), a fraction (`"3/4"`) or a mixed number (`"1 3/4"`).
#' @param ... Not used.
#' @param tolerance The largest relative error of the approximation.
#' @param max_d The largest denominator. If the tolerance isn't met, the last
#'   convergent with a smaller denominator is used. There may be a closer fraction
#'   which isn't a convergent, see [frac_approx()].
#'
#' @return A [fraction()] vector. Text which isn't a fraction is an error, except
#'   `NA` and empty strings which are missing. Text is stored as integers if
#'   possible.
#' @examples
#' as_fraction(c(0.1, 0.75, 1 / 3))
#' as_fraction(pi, max_d = 1000)
#' as_fraction(c("2/4", "3", "1 3/4"))
#'
#' as.double(fraction(1, 3))
#' @export
as_fraction <- function(x, ...) {
  UseMethod("as_fraction")
}

#' @export
as_fraction.default <- function(x, ...) {
  vec_cast(x, new_fraction())
}

#' @export
as_fraction.fraction <- function(x, ...) {
  x
}

#' @export
as_fraction.integer <- function(x, ...) {
  if (is.object(x)) return(as_fraction.default(x))
  fraction(x)
}

#' @rdname as_fraction
#' @export
as_fraction.double <- function(x, ..., tolerance = 1e-12, max_d = 1e9) {
  if (is.object(x)) return(as_fraction.default(x))
  check_number(tolerance, "tolerance", 0, 1)
  check_number(max_d, "max_d", 1, max_whole)
  x <- as_bare_number(x)
  frac_restore(.Call(C_frac_convergent, x, tolerance, max_d))
}

check_number <- function(x, arg, min, max, call = caller_env()) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min && x <= max
  if (!ok) {
    cli_abort("{.arg {arg}} must be a number between {min} and {max}.", call = call)
  }
}

#' @export
as_fraction.character <- function(x, ...) {
  parse_fraction(as.vector(x))
}

# Prototypes -------------------------------------------------------------------

is_integer_stored <- function(x) is.integer(field_n(x))

# Common type can't depend on values, so doubles (which may not be whole) win.
#' @export
vec_ptype2.fraction.fraction <- function(x, y, ...) {
  if (is_integer_stored(x)) new_fraction(field_n(y)[0], field_d(y)[0]) else vec_ptype(x)
}
#' @export
vec_ptype2.fraction.double <- function(x, y, ...) double()
#' @export
vec_ptype2.double.fraction <- function(x, y, ...) double()
#' @export
vec_ptype2.fraction.integer <- function(x, y, ...) vec_ptype(x)
#' @export
vec_ptype2.integer.fraction <- function(x, y, ...) vec_ptype(y)

# Casts ------------------------------------------------------------------------
# vctrs handles logical vectors of only missing values

#' @export
vec_cast.fraction.fraction <- function(x, to, ..., x_arg = "", to_arg = "",
                                       call = caller_env()) {
  if (is_integer_stored(x) == is_integer_stored(to)) return(x)
  type <- field_n(to)[0]
  new_fraction(
    vec_cast(field_n(x), type, x_arg = x_arg, to_arg = to_arg, call = call),
    vec_cast(field_d(x), type, x_arg = x_arg, to_arg = to_arg, call = call)
  )
}
#' @export
vec_cast.fraction.double <- function(x, to, ..., x_arg = "", to_arg = "",
                                     call = caller_env()) {
  x <- as_bare_number(x, "x", call = call)
  lossy <- !is_whole(x)
  out <- x
  out[lossy] <- NA
  out <- maybe_lossy_cast(
    fraction(out), x, to, lossy, x_arg = x_arg, to_arg = to_arg, call = call,
    details = "Use `as_fraction()` to approximate numbers which aren't whole."
  )
  vec_cast.fraction.fraction(out, to, x_arg = x_arg, to_arg = to_arg, call = call)
}
#' @export
vec_cast.fraction.integer <- function(x, to, ...) {
  vec_cast.fraction.fraction(fraction(x), to)
}
#' @export
vec_cast.fraction.character <- function(x, to, ..., x_arg = "x", to_arg = "",
                                        call = caller_env()) {
  out <- parse_fraction(x, to, x_arg = x_arg, to_arg = to_arg, call = call)
  vec_cast.fraction.fraction(out, to, x_arg = x_arg, to_arg = to_arg, call = call)
}
#' @export
vec_cast.double.fraction <- function(x, to, ...) frac_value(x)
#' @export
vec_cast.integer.fraction <- function(x, to, ..., x_arg = "", to_arg = "",
                                      call = caller_env()) {
  vec_cast(frac_value(x), integer(), x_arg = x_arg, to_arg = to_arg, call = call)
}
#' @export
vec_cast.character.fraction <- function(x, to, ...) frac_format(x)

#' @export
as.double.fraction <- function(x, ...) frac_value(x)

# Truncates, like base
#' @export
as.integer.fraction <- function(x, ...) as.integer(frac_value(trunc(x)))

#' @export
as.character.fraction <- function(x, ...) frac_format(x)
