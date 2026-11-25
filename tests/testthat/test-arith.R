test_that("exact and in lowest terms", {
  expect_terms(fraction(1, 3) + fraction(1, 6), 1, 2)
  expect_terms(fraction(1, 2) - fraction(3, 4), -1, 4)
  expect_terms(fraction(2, 4) * fraction(2, 4), 1, 4)
  expect_terms(fraction(1, 2) / fraction(-1, 4), -2, 1)
  expect_terms(fraction(1, 10) + fraction(2, 10), 3, 10)
  expect_terms(-fraction(2L, 4L), -2, 4)
  expect_identical(+fraction(2, 4), fraction(2, 4))
  x <- fraction(1:3, 4L)
  expect_identical(x + fraction(1L, 4L), simplify(fraction(2:4, 4L)))
  expect_error(x + fraction(1:2, 1L), class = "vctrs_error_incompatible_size")
  expect_length(x[0] + 1, 0)
})

test_that("agrees with doubles", {
  # Halves and quarters are exact doubles
  g <- expand.grid(a = -6:6, b = c(1, 2, 4), c = -6:6, e = c(1, 2, 4))
  x <- fraction(g$a, g$b)
  y <- fraction(g$c, g$e)
  for (op in c("+", "-", "*", "/", "%%", "%/%")) {
    f <- match.fun(op)
    out <- f(x, y)
    expect_equal(as.double(out), f(g$a / g$b, g$c / g$e), info = op)
    expect_identical(out, simplify(out))
  }
  for (op in c("==", "!=", "<", "<=", ">", ">=")) {
    f <- match.fun(op)
    expect_identical(f(x, y), f(g$a / g$b, g$c / g$e), info = op)
  }
  k <- rep_len(-3:3, length(x))
  expect_equal(as.double(x^k), as.double(x)^k)
})

test_that("special values agree with doubles", {
  v <- c(-Inf, -1.5, 0, 2, Inf, NaN, NA)
  g <- expand.grid(a = v, b = v)
  x <- as_fraction(g$a)
  y <- as_fraction(g$b)
  for (op in c("+", "-", "*", "/", "%/%", "%%", "==", "<", ">=")) {
    f <- match.fun(op)
    # Plain NA and NaN operands mix in platform dependent ways
    keep <- !(is.nan(g$a) & is.na(g$b)) & !(is.nan(g$b) & is.na(g$a))
    expect_equal(as.double(f(x, y))[keep], as.double(f(g$a, g$b))[keep], info = op)
  }
  k <- rep(c(-2, -1, 0, 1, 2, NA), each = length(v))
  a <- rep_len(v, length(k))
  expect_equal(as.double(as_fraction(a)^k), a^k)
})

test_that("plain numbers", {
  x <- fraction(1L, c(2L, 4L))
  expect_terms(x + 1L, c(3, 5), c(2, 4))
  expect_type(numerator(x + 1L), "integer")
  expect_type(numerator(x + 1), "double")
  expect_terms(1 - x, c(1, 3), c(2, 4))
  expect_terms(x * NA_real_, c(NA, NA), c(NA, NA))
  expect_terms(x + NA, c(NA, NA), c(NA, NA))
  # Any element which isn't whole
  expect_identical(x + 0.5, c(1, 0.75))
  expect_identical(x + c(1, 0.5), c(1.5, 0.75))
  expect_identical(0.5 / x, c(1, 2))
  expect_identical(x + Inf, c(Inf, Inf))
  expect_identical(x + 2^60, c(2^60, 2^60))
  for (bad in list(TRUE, "a", Sys.Date())) {
    suppressWarnings(expect_error(x + bad))
  }
  expect_error(x & x, class = "vctrs_error_incompatible_op")
  expect_error(!x, class = "vctrs_error_incompatible_op")
})

test_that("powers", {
  expect_terms(fraction(2, 3)^c(2, -2, 0), c(4, 9, 1), c(9, 4, 1))
  expect_terms(fraction(2, 4)^fraction(4, 2), 1, 4)
  expect_terms(2^fraction(3), 8, 1)
  expect_terms(fraction(-1)^c(1e9, 1e9 + 1), c(1, -1), c(1, 1))
  expect_terms(fraction(1, 2)^53, 1, 2^53)
  expect_identical(fraction(1, 4)^0.5, 0.5)
  expect_identical(4^fraction(1, 2), 2)
  expect_warning(expect_true(is.na(fraction(1, 2)^54)), "too large")
})

test_that("overflow", {
  big <- .Machine$integer.max
  x <- fraction(big, 1L)
  expect_type(numerator(x + 0L), "integer")
  expect_type(numerator(x + 1L), "double")
  expect_terms(x + 1L, 2^31, 1)
  expect_terms(fraction(1, 2^53) * fraction(2^53, 1), 1, 1)
  expect_warning(y <- fraction(c(1, 2^53)) + 1, "too large")
  expect_terms(y, c(2, NA), c(1, NA))
  # Only totals need to be representable
  expect_terms(sum(fraction(c(2^53, 2^53, -2^53))), 2^53, 1)
  expect_terms(sum(fraction(1, 1:40)), 2078178381193813, 485721041551200)
  expect_warning(expect_true(is.na(sum(fraction(1, 1:60)))), "too large")
  expect_silent(sum(fraction(c(1, NA), 2)))
  # Totals are less than 2^127 in size
  p <- fraction(c(2^53, 2^53, 2^21, 1, 1), c(1, 1, 1, 2^53, 2^53))
  expect_terms(prod(p / c(1, 1, 2, 1, 1)), 2^20, 1)
  expect_warning(expect_true(is.na(prod(p))), "too large")
  expect_warning(expect_true(is.na(prod(p * c(-1, 1, 1, 1, 1)))), "too large")
  # Zero is reduced
  expect_terms(sum(fraction(c(1, -1), 3)), 0, 1)
  expect_terms(prod(fraction(c(1, 0), 3)), 0, 1)
})

test_that("summaries", {
  x <- fraction(c(2L, 1L, 3L), c(4L, 3L, 4L))
  expect_terms(sum(x), 19, 12)
  expect_terms(prod(x), 1, 8)
  expect_terms(mean(x), 19, 36)
  expect_terms(median(x), 1, 2)
  expect_terms(median(x[2:3]), 13, 24)
  expect_terms(min(x), 1, 3)
  expect_terms(max(x), 3, 4)
  expect_terms(range(x), c(1, 3), c(3, 4))
  expect_terms(sum(x, 1L), 31, 12)
  expect_terms(sum(x, 1), 31, 12)
  expect_equal(sum(x, 0.5), 19 / 12 + 0.5)
  expect_identical(quantile(x, 0.5), c(`50%` = 0.5))
  expect_terms(cumsum(x), c(1, 5, 19), c(2, 6, 12))
  expect_terms(cumprod(x), c(1, 1, 1), c(2, 6, 8))
  expect_terms(cummin(x), c(1, 1, 1), c(2, 3, 3))
  expect_terms(cummax(x), c(1, 1, 3), c(2, 2, 4))
  expect_error(any(x), "not supported")

  expect_terms(sum(x[0]), 0, 1)
  expect_terms(prod(x[0]), 1, 1)
  expect_terms(mean(x[0]), 0, 0)
  expect_warning(expect_terms(min(x[0]), 1, 0))
  expect_warning(expect_terms(range(x[0]), c(1, -1), c(0, 0)))

  y <- c(x, NA)
  for (f in list(sum, prod, mean, median, min, max)) {
    expect_true(is.na(f(y)))
    expect_identical(f(y, na.rm = TRUE), f(x))
  }
  expect_identical(is.na(cumsum(c(y, x))), rep(c(FALSE, TRUE), c(3, 4)))
  expect_identical(is.na(cummax(c(y, x))), rep(c(FALSE, TRUE), c(3, 4)))
})

test_that("rounding", {
  v <- c(-2.5, -1.75, -0.5, 0, 0.25, 0.5, 1.5, 3)
  x <- as_fraction(v)
  for (f in list(floor, ceiling, trunc, round, abs, sign)) {
    expect_equal(as.double(f(x)), f(v))
  }
  expect_terms(round(fraction(c(1, 2, 5), 3), 2), c(33, 67, 167), rep(100, 3))
  expect_terms(round(fraction(1235), -1), 1240, 1)
  expect_terms(round(fraction(1, 3), c(1)), 3, 10)
  expect_terms(round(fraction(5), -400), 0, 1)
  expect_error(round(x, 16), "15")
  expect_error(round(x, NA), "missing")
  expect_terms(sign(fraction(c(-1, 0, NA, 0), c(0, 0, 1, 2))), c(-1, 0, NA, 0), c(1, 0, NA, 1))
})

test_that("other maths gives doubles", {
  x <- fraction(1, 4)
  expect_identical(sqrt(x), 0.5)
  expect_identical(exp(x), exp(0.25))
  expect_identical(log(x, 2), -2)
  expect_identical(signif(fraction(1, 3), 2), 0.33)
  expect_identical(is.finite(fraction(c(1, 1), c(2, 0))), c(TRUE, FALSE))
  expect_identical(is.nan(fraction(c(0, NA), 0)), c(TRUE, FALSE))
})
