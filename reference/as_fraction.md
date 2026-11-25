# Convert to fractions

Whole numbers are converted exactly. Other numbers are approximated with
the first continued fraction convergent within a tolerance, since
numbers such as 0.1 are not stored exactly by computers.

## Usage

``` r
as_fraction(x, ...)

# S3 method for class 'double'
as_fraction(x, ..., tolerance = 1e-12, max_d = 1e+09)
```

## Arguments

- x:

  A numeric, character or
  [`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
  vector. Text can be a whole number (`"3"`), a fraction (`"3/4"`) or a
  mixed number (`"1 3/4"`).

- ...:

  Not used.

- tolerance:

  The largest relative error of the approximation.

- max_d:

  The largest denominator. If the tolerance isn't met, the last
  convergent with a smaller denominator is used. There may be a closer
  fraction which isn't a convergent, see
  [`frac_approx()`](https://pkg.mitchelloharawild.com/rhind/reference/frac_approx.md).

## Value

A
[`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
vector. Text which isn't a fraction is an error, except `NA` and empty
strings which are missing. Text is stored as integers if possible.

## Examples

``` r
as_fraction(c(0.1, 0.75, 1 / 3))
#> <fraction<double>[3]>
#> [1] 1/10  3/4  1/3
as_fraction(pi, max_d = 1000)
#> <fraction<double>[1]>
#> [1] 355/113
as_fraction(c("2/4", "3", "1 3/4"))
#> <fraction<integer>[3]>
#> [1] 2/4   3 7/4

as.double(fraction(1, 3))
#> [1] 0.3333333
```
