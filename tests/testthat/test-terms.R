test_that("frac_rescale()", {
  expect_terms(frac_rescale(fraction(3, 4), 100), 75, 100)
  x <- fraction(c(2L, -1L, 0L, 3L), c(4L, 3L, 5L, 1L))
  expect_terms(frac_rescale(x, 6L), c(3, -2, 0, 18), rep(6, 4))
  expect_terms(frac_rescale(x, c(2L, 3L, 1L, 2L)), c(1, -1, 0, 6), c(2, 3, 1, 2))
  expect_terms(frac_rescale(fraction(1, 2), c(2, NA, 4)), c(1, NA, 2), c(2, NA, 4))
  # Already has this denominator
  expect_identical(frac_rescale(fraction(2, 4), 4), fraction(2, 4))

  expect_identical(frac_rescale(x[0], 1L), x[0])
  expect_identical(frac_rescale(x[1], integer()), x[0])
  y <- fraction(c(NA, 5, -5, 0), c(1, 0, 0, 0))
  expect_identical(frac_rescale(y, 10), y)
  expect_identical(frac_rescale(y, c(1, NA, 1, NA)), y[c(1, 1, 3, 1)])
})

test_that("frac_rescale() storage", {
  x <- fraction(1L, 2L)
  expect_type(frac_num(frac_rescale(x, 4L)), "integer")
  expect_type(frac_num(frac_rescale(x, NA)), "integer")
  expect_type(frac_num(frac_rescale(x, 4)), "double")
  expect_type(frac_num(frac_rescale(fraction(1, 2), 4L)), "double")
  big <- .Machine$integer.max
  expect_type(frac_num(frac_rescale(fraction(big), 1L)), "integer")
  expect_identical(frac_rescale(fraction(big), 2L), fraction(2 * as.double(big), 2))

  expect_terms(frac_rescale(fraction(1, 2^53), 2^53), 1, 2^53)
  expect_terms(frac_rescale(fraction(-2^52), 2), -2^53, 2)
  expect_terms(frac_rescale(fraction(2^53, 2^53), 2^53), 2^53, 2^53)
  # 3 * (2^53 + 1) / 3 would be rounded to 2^53 as doubles
  expect_warning(
    expect_terms(
      frac_rescale(fraction(c(-3002399751580331, 1, 2^53)), c(3, 3, 2^53)),
      c(NA, 3, NA), c(NA, 3, NA)
    ),
    "too large"
  )
})

test_that("frac_rescale() checks inputs", {
  x <- fraction(c(1, 2, 1, 1), c(2, 4, 3, 7))
  expect_error(frac_rescale(x, 100), "1/3.*100.*location 3")
  expect_error(frac_rescale(x, c(2, 2, 3, 8)), "1/7.*8.*location 4")
  expect_error(frac_rescale(fraction(1, 3), c(3, 4)), "1/3.*4.*location 2")
  expect_error(frac_rescale(fraction(2^53 - 1, 2^53), 2^52), "location 1")
  expect_error(frac_rescale(x, 1:2), class = "vctrs_error_incompatible_size")
  for (bad in list(0, -2, c(1, 1, 1, 0))) {
    expect_error(frac_rescale(x, bad), "positive")
  }
  for (bad in list(1.5, Inf, 2^54)) {
    expect_error(frac_rescale(x, bad), "whole numbers")
  }
  expect_error(frac_rescale(x, "2"), class = "vctrs_error_incompatible_type")
  expect_error(frac_rescale(0.5, 2), "fraction")
  expect_error(frac_rescale(x))
})

test_that("frac_align()", {
  expect_terms(frac_align(fraction(1, c(2, 3))), c(3, 2), c(6, 6))
  x <- fraction(c(2L, -1L, 0L, 5L, 2L), c(8L, 6L, 7L, 1L, 2L))
  out <- frac_align(x)
  expect_terms(out, c(3, -2, 0, 60, 12), rep(12, 5))
  expect_type(frac_num(out), "integer")
  expect_identical(frac_align(out), out)
  expect_terms(frac_align(fraction(c(2, 0), c(4, 4))), c(1, 0), c(2, 2))

  # Missing and special values
  expect_terms(
    frac_align(fraction(c(1, NA, 5, -5, 0, 1), c(2, 7, 0, 0, 0, 3))),
    c(3, NA, 5, -5, 0, 2), c(6, NA, 0, 0, 0, 6)
  )
  y <- fraction(c(NA, 5L, 0L), c(1L, 0L, 0L))
  expect_identical(frac_align(y), y)
  expect_identical(frac_align(x[0]), x[0])
  expect_identical(frac_align(fraction(double())), fraction(double()))
  expect_error(frac_align(1), "fraction")
})

test_that("frac_align() limits", {
  big <- .Machine$integer.max
  x <- fraction(1L, c(big, 2L))
  expect_identical(frac_align(x), fraction(c(2, big), 2 * as.double(big)))
  # Only numerators are too large for integers
  expect_identical(
    frac_align(fraction(c(big, 1L), c(1L, 2L))), fraction(c(2 * as.double(big), 1), 2)
  )
  expect_terms(frac_align(fraction(1, c(2^53, 2))), c(1, 2^52), c(2^53, 2^53))
  expect_error(frac_align(fraction(1, c(2^53, 3))), "least common denominator.*2\\^53")
  # 2^53 + 1 = 3 * 3002399751580331 would be rounded to 2^53 as doubles
  expect_error(frac_align(fraction(1, c(3, 3002399751580331))), "least common denominator")
  expect_error(frac_align(fraction(1, 1:60)), "least common denominator")
  expect_warning(
    expect_terms(frac_align(fraction(c(2^53, 1), c(1, 3))), c(NA, 1), c(NA, 3)),
    "too large"
  )
})

test_that("frac_approx()", {
  expect_terms(frac_approx(fraction(355, 113), 10), 22, 7)
  # Semiconvergents, which aren't found by as_fraction()
  expect_terms(frac_approx(as_fraction(pi), 100), 311, 99)
  expect_terms(as_fraction(pi, max_d = 100), 22, 7)
  x <- fraction(c(2L, -4L, 0L, 100L, 1L), c(4L, 6L, 9L, 1L, 4L))
  out <- frac_approx(x, 3)
  expect_terms(out, c(1, -2, 0, 100, 1), c(2, 3, 1, 1, 3))
  expect_type(frac_num(out), "integer")
  expect_type(frac_num(frac_approx(fraction(1, 4), 3L)), "double")
  # Ties are the smaller denominator, then towards zero
  expect_terms(frac_approx(fraction(c(5, -5), 12), 3), c(1, -1), c(2, 2))
  expect_terms(frac_approx(fraction(c(-3, -1, 1, 3, 5), 2), 1), c(-1, 0, 0, 1, 2), rep(1, 5))

  expect_terms(frac_approx(fraction(c(NA, 1, -1, 0), c(1, 0, 0, 0)), 5), c(NA, 1, -1, 0), c(NA, 0, 0, 0))
  expect_identical(frac_approx(x[0], 5), x[0])

  # Not distinguishable as doubles
  x <- fraction(c(2^53 - 1, 1 - 2^53, 1, 2^53 - 1), c(2^53, 2^53, 2^53, 2))
  expect_terms(
    frac_approx(x, 2^53 - 1),
    c(2^53 - 2, 2 - 2^53, 1, 2^53 - 1), c(2^53 - 1, 2^53 - 1, 2^53 - 1, 2)
  )
  expect_terms(frac_approx(x, 1), c(1, -1, 0, 2^52 - 1), rep(1, 4))
  expect_identical(frac_approx(x, 2^53), x)

  for (bad in list(0, -1, NA, Inf, 2^54, 1.5, 1:2, numeric(), "2", NULL)) {
    expect_error(frac_approx(x, bad), "max_d")
  }
  expect_error(frac_approx(0.5, 2), "fraction")
})

test_that("frac_approx() is the closest of all denominators", {
  max <- 12
  g <- expand.grid(n = 0:50, d = 1:25)
  # In order of preference. Errors are multiples of 1 / (d * lcd).
  lcd <- 27720
  best <- list(n = g$n, d = g$d, error = rep(Inf, nrow(g)))
  for (q in seq_len(max)) {
    for (p in list((g$n * q) %/% g$d, (g$n * q) %/% g$d + 1)) {
      error <- abs(g$n * q - p * g$d) * (lcd / q)
      closer <- error < best$error
      best$n[closer] <- p[closer]
      best$d[closer] <- q
      best$error[closer] <- error[closer]
    }
    expect_terms(frac_approx(fraction(g$n, g$d), q), best$n, best$d)
    expect_terms(frac_approx(fraction(-g$n, g$d), q), -best$n, best$d)
  }
})
