#' @export
Math.fraction <- function(x, ...) {
  vec_math(.Generic, x, ...)
}

# Arguments are combined, as fractions if plain numbers are whole
#' @export
Summary.fraction <- function(..., na.rm = FALSE) {
  dots <- list(...)
  if (length(dots) == 1L) {
    return(vec_math(.Generic, dots[[1L]], na.rm = na.rm))
  }
  x <- do.call(vec_c, lapply(dots, operand, arg = "..."))
  if (is_fraction(x)) {
    vec_math(.Generic, x, na.rm = na.rm)
  } else {
    get(.Generic, baseenv())(x, na.rm = na.rm)
  }
}

#' @export
vec_math.fraction <- function(.fn, .x, ...) {
  switch(
    .fn,
    sum = , prod = , mean = math_total(.fn, .x, ...),
    min = , max = , range = math_extreme(.fn, .x, ...),
    cumsum = frac_accumulate(.x, "+", TRUE),
    cumprod = frac_accumulate(.x, "*", TRUE),
    cummin = cumulative_extreme(.x, 1L),
    cummax = cumulative_extreme(.x, -1L),
    floor = round_fraction(.x, 1L),
    ceiling = round_fraction(.x, 2L),
    trunc = round_fraction(.x, 3L),
    round = math_round(.x, ...),
    abs = simplify(new_fraction(abs(field_n(.x)), field_d(.x))),
    sign = math_sign(.x),
    any = , all = {
      cli_abort("{.fn {(.fn)}} is not supported for {.cls fraction} vectors.", call = NULL)
    },
    # Includes is.nan(), is.finite() and is.infinite()
    get(.fn, baseenv())(frac_value(.x), ...)
  )
}

drop_missing <- function(x, na.rm) {
  if (na.rm && anyNA(x)) x[!is.na(x)] else x
}

frac_accumulate <- function(x, op, cumulative, by = 1) {
  frac_restore(.Call(
    C_frac_accumulate, op_codes[[op]], field_n(x), field_d(x), cumulative, by
  ))
}

math_total <- function(.fn, .x, ..., na.rm = FALSE) {
  .x <- drop_missing(.x, na.rm)
  frac_accumulate(
    .x, if (.fn == "prod") "*" else "+", FALSE,
    by = if (.fn == "mean") as.double(length(.x)) else 1
  )
}

extreme <- function(x, sign, cumulative) {
  .Call(C_frac_extreme, field_n(x), field_d(x), sign, cumulative)
}

cumulative_extreme <- function(x, sign) {
  simplify(x[extreme(x, sign, TRUE)])
}

math_extreme <- function(.fn, .x, ..., na.rm = FALSE) {
  .x <- drop_missing(.x, na.rm)
  if (length(.x) == 0L) {
    # As infinite, with the warnings of base
    limits <- c(Inf, -Inf)
    warning("no non-missing arguments; returning infinite fractions", call. = FALSE)
    n <- switch(.fn, min = 1L, max = -1L, range = as.integer(sign(limits)))
    return(vec_cast(fraction(n, 0L), vec_ptype(.x)))
  }
  at <- c(
    if (.fn != "max") extreme(.x, 1L, FALSE),
    if (.fn != "min") extreme(.x, -1L, FALSE)
  )
  simplify(.x[at])
}

# Modes are floor, ceiling, trunc, and round (half to even)
round_fraction <- function(x, mode, digits = 0L) {
  frac_restore(.Call(C_frac_round, field_n(x), field_d(x), mode, digits))
}

math_round <- function(x, digits = 0, ...) {
  digits <- as_bare_number(digits, "digits")
  if (anyNA(digits)) cli_abort("{.arg digits} can't be missing.")
  vec_recycle(digits, length(x), x_arg = "digits")
  # With more digits, scaling would overflow. With fewer, all are zero.
  digits <- floor(digits + 0.5)
  if (any(digits > 15)) {
    cli_abort("{.arg digits} can't be larger than 15.")
  }
  # All fractions are less than half of 10^17
  round_fraction(x, 4L, as.integer(pmax(digits, -17)))
}

math_sign <- function(x) {
  n <- sign(field_n(x))
  nan <- which(field_n(x) == 0L & field_d(x) == 0L)
  if (is_integer_stored(x)) n <- as.integer(n)
  d <- n
  d[!is.na(d)] <- 1L
  d[nan] <- 0L
  storage.mode(d) <- storage.mode(n)
  new_fraction(n, d)
}

#' @export
mean.fraction <- function(x, ..., na.rm = FALSE) {
  vec_math("mean", x, na.rm = na.rm)
}

#' @export
median.fraction <- function(x, na.rm = FALSE, ...) {
  x <- drop_missing(x, na.rm)
  n <- length(x)
  if (n == 0L || anyNA(x)) return(vec_init(x, 1L))
  mid <- sort(x)[unique(c((n + 1L) %/% 2L, n %/% 2L + 1L))]
  mean(mid)
}

#' @export
quantile.fraction <- function(x, ...) {
  quantile(frac_value(x), ...)
}
