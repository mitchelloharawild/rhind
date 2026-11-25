test_that("approximates doubles", {
  x <- c(0.1, 0.75, 1 / 3, -2 / 7, 5, 0, NA, Inf, -Inf, NaN)
  expect_terms(
    as_fraction(x),
    c(1, 3, 1, -2, 5, 0, NA, 1, -1, 0), c(10, 4, 3, 7, 1, 1, NA, 0, 0, 0)
  )
  expect_terms(as_fraction(pi, max_d = 1000), 355, 113)
  expect_terms(as_fraction(pi, tolerance = 0.01), 22, 7)
  expect_equal(as.double(as_fraction(pi)), pi)
  expect_warning(expect_true(is.na(as_fraction(1e300))), "too large")
  expect_error(as_fraction(0.5, max_d = 0), "max_d")
  expect_error(as_fraction(0.5, tolerance = NA), "tolerance")
  r <- c(1e-5, 0.123456, 12345.678)
  expect_equal(as.double(as_fraction(r)), r, tolerance = 1e-12)
})

test_that("other types", {
  expect_identical(as_fraction(1:2), fraction(1:2))
  x <- fraction(2, 4)
  expect_identical(as_fraction(x), x)
  expect_identical(as_fraction(NA), fraction(NA))
  expect_error(as_fraction(TRUE), class = "vctrs_error_cast")
  expect_error(as_fraction(Sys.Date()), class = "vctrs_error_cast")
  expect_error(as_fraction(factor(1)), class = "vctrs_error_cast")
})

test_that("text", {
  x <- as_fraction(c("2/4", " -3 / 4 ", "3", "1 3/4", "-1 1/2", "+1/2", "1/0", NA, ""))
  expect_terms(x, c(2, -3, 3, 7, -3, 1, 1, NA, NA), c(4, 4, 1, 4, 2, 2, 0, NA, NA))
  expect_type(frac_num(x), "integer")
  expect_type(frac_num(as_fraction("3000000000/7")), "double")
  for (bad in c("a", "1/", "/2", "1.5", "1/2/3", "1 2", "1/-2", "--1", "99999999999999999")) {
    expect_error(as_fraction(bad), class = "vctrs_error_cast")
  }
  expect_length(as_fraction(character()), 0)
  f <- c("2/4", "-1/3", "5", "0/0")
  expect_identical(format(as_fraction(f)), f)
})

test_that("text up to 2^53", {
  x <- fraction(c(2^53, -2^53, 1, 2^53, -2^53), c(3, 3, 2^53, 1, 2^53))
  expect_identical(as_fraction(format(x)), x)
  max <- "9007199254740992"
  expect_terms(
    as_fraction(paste0(c("00", "+000", "-0"), max, c("", "/03", "/0009007199254740992"))),
    c(2^53, 2^53, -2^53), c(1, 3, 2^53)
  )
  # Mixed numbers
  expect_terms(
    as_fraction(c(
      "0 9007199254740992/9007199254740992", "9007199254740992 0/1",
      "-1 9007199254740991/1", "1 4503599627370496/4503599627370496",
      "1 9007199254740992/0", "9007199254740992 1/0"
    )),
    c(2^53, 2^53, -2^53, 2^53, 2^53, 1),
    c(2^53, 1, 1, 2^52, 0, 0)
  )
  for (large in c("9007199254740993", "009007199254740993", "9007199254740994",
                  "10000000000000000", "99999999999999999", strrep("9", 400))) {
    for (bad in paste0(
      c("", "-", "", "1/", "", "", "0 ", "0 1/"),
      large, c("", "", "/3", "", " 0/1", " 1/0", "/3", "")
    )) {
      expect_error(as_fraction(bad), class = "vctrs_error_cast")
    }
  }
  # Totals of 2^53 + 1 and 2^53 + 2, from terms up to 2^53
  for (bad in c(
    "9007199254740991 2/1", "9007199254740992 1/1", "1 9007199254740992/1",
    "3002399751580331 0/3", "-3002399751580331 1/3", "1 1/9007199254740992"
  )) {
    expect_error(as_fraction(bad), class = "vctrs_error_cast")
  }
})

test_that("to other types", {
  x <- fraction(c(1, 3, NA, 1), c(2, 1, 1, 0))
  expect_identical(as.double(x), c(0.5, 3, NA, Inf))
  expect_identical(as.character(x), c("1/2", "3", NA, "1/0"))
  expect_identical(as.integer(fraction(c(7, -7, NA), 2)), c(3L, -3L, NA))
  expect_identical(vctrs::vec_cast(fraction(4, 2), integer()), 2L)
  expect_error(vctrs::vec_cast(fraction(1, 2), integer()), class = "vctrs_error_cast_lossy")
  expect_error(vctrs::vec_cast(0.5, fraction()), class = "vctrs_error_cast_lossy")
})

test_that("common types", {
  int <- fraction()
  dbl <- fraction(double())
  ptype <- function(...) vctrs::vec_ptype_common(...)
  expect_identical(ptype(int, int), int)
  expect_identical(ptype(int, dbl), dbl)
  expect_identical(ptype(dbl, int), dbl)
  expect_identical(ptype(int, 1L), int)
  expect_identical(ptype(1L, dbl), dbl)
  expect_identical(ptype(int, NA), int)
  expect_identical(ptype(int, 1), double())
  expect_identical(ptype(1, dbl), double())
  expect_error(ptype(int, "a"), class = "vctrs_error_incompatible_type")
  expect_identical(c(fraction(1L, 2L), 1L), fraction(c(1L, 1L), c(2L, 1L)))
  expect_identical(c(fraction(1L, 2L), 0.25), c(0.5, 0.25))
})

test_that("format and print", {
  x <- fraction(c(2L, 3L, NA), c(4L, 1L, 1L))
  expect_identical(format(x), c("2/4", "3", "NA"))
  expect_output(print(x), "fraction<integer>[3]", fixed = TRUE)
  expect_output(print(x, max = 2), "omitted 1")
  expect_output(print(fraction(1, 2)), "fraction<double>[1]", fixed = TRUE)
  expect_identical(format(fraction(2^53, 3)), "9007199254740992/3")
  skip_if_not_installed("tibble")
  expect_output(print(tibble::tibble(x)), "<frac>")
})
