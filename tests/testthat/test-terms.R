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
