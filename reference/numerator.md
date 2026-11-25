# Components of fractions

Components of fractions

## Usage

``` r
numerator(x)

denominator(x)

simplify(x)
```

## Arguments

- x:

  A
  [`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
  vector.

## Value

- `numerator()` and `denominator()`: integer or double vectors, as
  stored.

- `simplify()`: `x` in lowest terms.

## Examples

``` r
x <- fraction(c(2, 30), c(4, 100))
numerator(x)
#> [1]  2 30
denominator(x)
#> [1]   4 100
simplify(x)
#> <fraction<double>[2]>
#> [1]  1/2 3/10
```
