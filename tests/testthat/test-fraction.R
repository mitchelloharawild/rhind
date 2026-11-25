test_that("constructs", {
  x <- fraction(c(2, 1), c(4, 3))
  expect_s3_class(x, c("fraction", "vctrs_rcrd", "vctrs_vctr"), exact = TRUE)
  expect_true(is_fraction(x))
  expect_false(is_fraction(0.5))
  expect_identical(frac_num(x), c(2, 1))
  expect_identical(frac_den(x), c(4, 3))
  expect_length(fraction(), 0)
  expect_identical(fraction(1L, 1:3), fraction(c(1L, 1L, 1L), 1:3))
  expect_identical(fraction(c(a = 1), 2), fraction(1, 2))
  expect_error(fraction(1:2, 1:3), class = "vctrs_error_incompatible_size")
  expect_error(fraction("a"), class = "vctrs_error_incompatible_type")
  expect_error(fraction(1.5, 2), "whole numbers")
  expect_error(fraction(Inf, 2), "whole numbers")
  expect_error(fraction(2^54, 1), "whole numbers")
  expect_error(fraction(matrix(1:4, 2)), "dimensions")
  expect_error(new_fraction(1L, 1), "same type")
  expect_error(new_fraction(1, c(1, 2)), "same size")
  expect_error(new_fraction("1", "2"), "numbers")
  expect_error(frac_num(1), "fraction")
})

test_that("storage follows inputs", {
  expect_type(frac_num(fraction(1L, 2L)), "integer")
  expect_type(frac_den(fraction(1L)), "integer")
  expect_type(frac_num(fraction(1L, 2)), "double")
  expect_type(frac_den(fraction(1, 2L)), "double")
})

test_that("sign is in the numerator", {
  expect_terms(fraction(c(1, -1, 0), c(-2, -2, -2)), c(-1, 1, 0), c(2, 2, 2))
  expect_terms(fraction(1L, -2L), -1, 2)
  expect_identical(1 / frac_num(fraction(0, -2)), Inf)
})

test_that("missing values", {
  x <- fraction(c(1L, NA, 1L), c(2L, 2L, NA))
  expect_identical(is.na(x), c(FALSE, TRUE, TRUE))
  expect_true(anyNA(x))
  expect_identical(frac_num(x), c(1L, NA, NA))
  expect_identical(frac_den(x), c(2L, NA, NA))
  expect_identical(is.na(fraction(NA)), TRUE)
  expect_identical(is.na(fraction(c(1, 0), 0)), c(FALSE, TRUE))
})

test_that("frac_simplify()", {
  x <- fraction(c(2L, -30L, 0L, 5L, -5L, 0L, NA), c(4L, 100L, 7L, 0L, 0L, 0L, 1L))
  expect_terms(frac_simplify(x), c(1, -3, 0, 1, -1, 0, NA), c(2, 10, 1, 0, 0, 0, NA))
  expect_type(frac_num(frac_simplify(x)), "integer")
})

test_that("subset and assign keep terms", {
  x <- fraction(2:4, 4L)
  expect_terms(x[2:3], c(3, 4), c(4, 4))
  x[1] <- 5
  expect_terms(x[1], 5, 1)
  x[2] <- fraction(1, 7)
  expect_terms(x[2], 1, 7)
  x[5] <- 1L
  expect_identical(is.na(x), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_error(x[1] <- 0.5, class = "vctrs_error_cast_lossy")
  expect_error(x[1] <- 3e9, class = "vctrs_error_cast_lossy")
  expect_identical(rep(fraction(1, 2), 2), fraction(c(1, 1), 2))
})

test_that("data frames", {
  skip_if_not_installed("dplyr")
  df <- dplyr::tibble(x = fraction(c(2, 1, 1), c(4, 2, 3)), y = 1:3)
  out <- dplyr::summarise(dplyr::group_by(df, x), y = sum(y))
  expect_identical(out$y, c(3L, 3L))
  expect_identical(dplyr::arrange(df, x)$y, c(3L, 1L, 2L))
  expect_length(rbind(data.frame(x = df$x), data.frame(x = df$x))$x, 6)
})
