# Format fractions

Fractions are displayed as they are stored, without reducing to lowest
terms. Denominators of 1 are omitted.

## Usage

``` r
# S3 method for class 'fraction'
format(x, ...)
```

## Arguments

- x:

  A
  [`fraction()`](https://pkg.mitchelloharawild.com/fraction/reference/fraction.md)
  vector.

- ...:

  Not used.

## Value

An unnamed character vector the size of `x`. Missing values are `"NA"`,
or `NA` with [`as.character()`](https://rdrr.io/r/base/character.html).

## Examples

``` r
format(fraction(c(1, 2, 3, NA), c(2, 4, 1, 1)))
#> [1] "1/2" "2/4" "3"   "NA" 
```
