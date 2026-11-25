# Same terms, regardless of storage
expect_terms <- function(x, n, d) {
  testthat::expect_s3_class(x, "fraction")
  testthat::expect_equal(as.double(numerator(x)), as.double(n))
  testthat::expect_equal(as.double(denominator(x)), as.double(d))
}
