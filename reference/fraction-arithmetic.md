# Arithmetic with fractions

Results are exact, and in lowest terms. Whole numbers are fractions, but
results are ordinary doubles if any element of a plain number isn't
whole, as these are usually not exact. Use
[`as_fraction()`](https://pkg.mitchelloharawild.com/fraction/reference/as_fraction.md)
to convert them first.

## Details

- Arithmetic:

  `+`, `-`, `*`, `/`, `%%` and `%/%` are exact. `^` is exact if all
  exponents are whole numbers, and otherwise gives doubles. Inputs must
  be the same size, or size 1.

- Summaries:

  [`sum()`](https://rdrr.io/r/base/sum.html),
  [`prod()`](https://rdrr.io/r/base/prod.html),
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`median()`](https://rdrr.io/r/stats/median.html),
  [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html) and
  [`range()`](https://rdrr.io/r/base/range.html) are exact.
  [`quantile()`](https://rdrr.io/r/stats/quantile.html) gives doubles.

- Cumulative:

  [`cumsum()`](https://rdrr.io/r/base/cumsum.html),
  [`cumprod()`](https://rdrr.io/r/base/cumsum.html),
  [`cummin()`](https://rdrr.io/r/base/cumsum.html) and
  [`cummax()`](https://rdrr.io/r/base/cumsum.html) are exact.

- Rounding:

  [`round()`](https://rdrr.io/r/base/Round.html),
  [`floor()`](https://rdrr.io/r/base/Round.html),
  [`ceiling()`](https://rdrr.io/r/base/Round.html) and
  [`trunc()`](https://rdrr.io/r/base/Round.html) are exact.
  [`round()`](https://rdrr.io/r/base/Round.html) gives the even digit
  for halves. [`signif()`](https://rdrr.io/r/base/Round.html) gives
  doubles.

- Other functions:

  [`abs()`](https://rdrr.io/r/base/MathFun.html) and
  [`sign()`](https://rdrr.io/r/base/sign.html) give fractions.
  [`exp()`](https://rdrr.io/r/base/Log.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html) and so on give
  doubles.

If a result needs numbers too large for integers, all results are stored
as doubles. Results which need numbers larger than 2^53 are missing,
with a warning. Use [`as.double()`](https://rdrr.io/r/base/double.html)
to calculate approximately instead.

Logical operators (`&`, `|`, `!`,
[`any()`](https://rdrr.io/r/base/any.html),
[`all()`](https://rdrr.io/r/base/all.html)) are not supported. Base R
finds methods for functions like
[`sum()`](https://rdrr.io/r/base/sum.html) with their first argument, so
use `sum(x, 1)` instead of `sum(1, x)`.

## Examples

``` r
fraction(1, 3) + fraction(1, 6)
#> <fraction<double>[1]>
#> [1] 1/2
fraction(2, 3) * 2
#> <fraction<double>[1]>
#> [1] 4/3
fraction(2, 3)^-2
#> <fraction<double>[1]>
#> [1] 9/4

# Not all whole numbers
fraction(1, 2) + 0.25
#> [1] 0.75
fraction(1, 2) + as_fraction(0.25)
#> <fraction<double>[1]>
#> [1] 3/4

sum(fraction(1, 1:10))
#> <fraction<double>[1]>
#> [1] 7381/2520
cumsum(fraction(1, c(2, 4, 8)))
#> <fraction<double>[3]>
#> [1] 1/2 3/4 7/8
```
