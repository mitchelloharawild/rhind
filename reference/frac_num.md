# Components of fractions

Components of fractions

## Usage

``` r
frac_num(x)

frac_den(x)

frac_simplify(x)
```

## Arguments

- x:

  A
  [`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
  vector.

## Value

- `frac_num()` and `frac_den()`: integer or double vectors, as stored.

- `frac_simplify()`: `x` in lowest terms.

## Examples

``` r
x <- fraction(c(2, 30), c(4, 100))
frac_num(x)
#> [1]  2 30
frac_den(x)
#> [1]   4 100
frac_simplify(x)
#> <fraction<double>[2]>
#> [1]  1/2 3/10
```
