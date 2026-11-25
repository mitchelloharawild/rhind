# Approximate with smaller denominators

The closest fractions with denominators no larger than `max_d`. Ties
give the smaller denominator, and whole numbers which tie (with
`max_d = 1`) are rounded towards zero.

## Usage

``` r
frac_approx(x, max_d)
```

## Arguments

- x:

  A
  [`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
  vector.

- max_d:

  The largest denominator, as a whole number.

## Value

A
[`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
vector in lowest terms, stored as integers if `x` is. Missing values and
fractions with a denominator of zero are not approximated.

## Details

This is calculated exactly, and considers all denominators up to
`max_d`. In comparison,
[`as_fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/as_fraction.md)
gives the first continued fraction convergent of a number which is
within a tolerance, so there may be a closer fraction.

## Examples

``` r
frac_approx(fraction(355, 113), 10)
#> <fraction<double>[1]>
#> [1] 22/7

# There are closer fractions than convergents
as_fraction(pi, max_d = 100)
#> <fraction<double>[1]>
#> [1] 22/7
frac_approx(as_fraction(pi), 100)
#> <fraction<double>[1]>
#> [1] 311/99
```
