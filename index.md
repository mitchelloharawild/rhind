# rhind

rhind provides vectors of exact fractions, with the numerator and
denominator stored separately as whole numbers.

- Fractions are displayed as they were written, so `3/10` and `30/100`
  remain distinct to read.
- Arithmetic, comparison and summaries are exact, with results in lowest
  terms.
- The components are integers or doubles, and integers become doubles
  when results need larger numbers.

## Installation

You can install the development version of fraction from GitHub:

``` r

# install.packages("pak")
pak::pak("mitchelloharawild/rhind")
```

## Usage

``` r

library(rhind)

fraction(1, 3) + fraction(1, 6)
#> <fraction<double>[1]>
#> [1] 1/2

x <- fraction(c(2, 30), c(4, 100))
x
#> <fraction<double>[2]>
#> [1]    2/4 30/100
simplify(x)
#> <fraction<double>[2]>
#> [1]  1/2 3/10

fraction(2, 4) == fraction(1, 2)
#> [1] TRUE
```

Whole numbers are fractions, and other numbers give doubles unless
converted with
[`as_fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/as_fraction.md).

``` r

fraction(1, 3) + 1
#> <fraction<double>[1]>
#> [1] 4/3
fraction(1, 3) + 0.5
#> [1] 0.8333333
fraction(1, 3) + as_fraction(0.5)
#> <fraction<double>[1]>
#> [1] 5/6
```

See
[`vignette("rhind")`](https://pkg.mitchelloharawild.com/rhind/articles/rhind.md)
for details.

## Caveats

- Results needing whole numbers larger than 2^53 are missing, with a
  warning.
- `c(x, 1)` gives doubles, use `c(x, 1L)` to combine as fractions.
- `fraction` vectors are records, so don’t have names or dimensions, and
  some base functions such as
  [`table()`](https://rdrr.io/r/base/table.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) don’t treat them as
  numbers.
