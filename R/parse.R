fraction_pattern <-
  "^([+-]?)(?:([0-9]+)[ \t]+(?=[0-9]+[ \t]*/))?([0-9]+)(?:[ \t]*/[ \t]*([0-9]+))?$"

parse_fraction <- function(x, to = new_fraction(), ..., x_arg = "x",
                           to_arg = "", call = caller_env()) {
  text <- trimws(x)
  text[!is.na(text) & text %in% c("", "NA")] <- NA_character_

  ok <- grepl(fraction_pattern, text, perl = TRUE)
  part <- function(i) {
    out <- rep_len("", length(text))
    out[ok] <- sub(fraction_pattern, paste0("\\", i), text[ok], perl = TRUE)
    out
  }
  number <- function(i, default) {
    digits <- part(i)
    out <- rep_len(default, length(text))
    out[digits != ""] <- as.double(digits[digits != ""])
    out
  }
  whole <- number(2, 0)
  n <- number(3, NA_real_)
  d <- number(4, 1)
  n <- whole * d + n
  n[part(1) == "-"] <- -n[part(1) == "-"]
  d[!ok] <- NA
  # Beyond 2^53 may have been rounded to 2^53
  large <- ok & (abs(n) >= max_whole | d >= max_whole | whole >= max_whole)

  bad <- !is.na(text) & (!ok | large)
  if (any(bad)) {
    stop_incompatible_cast(
      x, to, x_arg = x_arg, to_arg = to_arg, call = call,
      details = paste0(
        "Not a fraction (or too large): ",
        paste0('"', x[bad][seq_len(min(3L, sum(bad)))], '"', collapse = ", "),
        if (sum(bad) > 3L) ", ..."
      )
    )
  }
  if (all(abs(c(n, d)) <= .Machine$integer.max, na.rm = TRUE)) {
    n <- as.integer(n)
    d <- as.integer(d)
  }
  fraction(n, d)
}
