# Rewrite fractions with other denominators

`frac_rescale()` rewrites fractions with the denominator `d`, without
changing their value. `frac_align()` rewrites all fractions with their
least common denominator, the smallest which they can share.

## Usage

``` r
frac_rescale(x, d)

frac_align(x)
```

## Arguments

- x:

  A
  [`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
  vector.

- d:

  Denominators, as positive whole numbers. Must be the same size as `x`,
  or either can be size 1. Results are missing where `d` is missing, and
  it is an error if any fraction can't be written exactly with its
  denominator.

## Value

A
[`fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/fraction.md)
vector. As with arithmetic, it is stored as doubles if `x` or `d` are
doubles. It is an error if the least common denominator is larger than
2^53.

## Details

Missing fractions, and fractions with a denominator of zero, are kept as
they are. Results which need numbers larger than 2^53 are missing, with
a warning.

## See also

[`frac_simplify()`](https://pkg.mitchelloharawild.com/rhind/reference/frac_num.md)
for lowest terms.

## Examples

``` r
frac_rescale(fraction(3, 4), 100)
#> <fraction<double>[1]>
#> [1] 75/100
frac_rescale(fraction(c(1, 2), 3), c(6, 12))
#> <fraction<double>[2]>
#> [1]  2/6 8/12

frac_align(fraction(1, c(2, 3)))
#> <fraction<double>[2]>
#> [1] 3/6 2/6
```
