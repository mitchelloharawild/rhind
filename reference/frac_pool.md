# Pool fractions

The sum of the numerators over the sum of the denominators, which pools
proportions such as 3 of 10 and 30 of 100 into 33 of 110.

## Usage

``` r
frac_pool(x, na.rm = FALSE)
```

## Arguments

- x:

  A
  [`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
  vector.

- na.rm:

  Should missing values be removed?

## Value

A
[`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
of size 1, which isn't reduced to lowest terms. It is `0/0` if `x` is
empty, and missing if any elements of `x` are missing.

## Details

Unlike [`mean()`](https://rdrr.io/r/base/mean.html), which weights all
fractions equally, `frac_pool()` weights fractions by their
denominators. So it depends on how fractions are written: pooling `1/2`
with `0/1` is `1/3`, but pooling `2/4` with `0/1` is `2/5`. Calculations
reduce fractions to lowest terms, so pool the original fractions.

`0/0` contributes nothing to the total, and `1/0` or `-1/0` give the
same result (or `0/0` if there are both). Totals larger than 2^53 are
missing, with a warning.

## Examples

``` r
x <- fraction(c(3, 30, 1), c(10, 100, 2))
frac_pool(x)
#> <fraction<double>[1]>
#> [1] 34/112
mean(x)
#> <fraction<double>[1]>
#> [1] 11/30
```
