# Exact fractions

A `fraction` vector stores the numerator and denominator of each element
separately, as whole numbers. Fractions are kept as they were written,
so `fraction(2, 4)` is `2/4`, while results of
[calculations](https://pkg.mitchelloharawild.com/fraction/reference/fraction-arithmetic.md)
are exact and in lowest terms.

`new_fraction()` is a low-level constructor for developers. It requires
`n` and `d` to be of the same type (integer or double) and size, with
non-negative `d`, and doesn't check their values.

## Usage

``` r
fraction(n = integer(), d = 1L)

new_fraction(n = integer(), d = integer())

is_fraction(x)
```

## Arguments

- n, d:

  For `fraction()` and `new_fraction()`, the numerator and denominator
  as whole numbers. `fraction()` recycles inputs of size 1. If either is
  missing, the fraction is missing.

- x:

  An object to test.

## Value

`fraction()` and `new_fraction()` return a `fraction` vector.
`is_fraction()` returns `TRUE` or `FALSE`.

## Details

The numerator and denominator are either both integers, or both doubles
if either input is a double. Integers use half the memory, and become
doubles if a result needs larger numbers. Doubles are limited to whole
numbers up to 2^53 (about 9e15).

The sign is stored in the numerator. A denominator of zero gives `1/0`,
`-1/0` and `0/0`, which behave as `Inf`, `-Inf` and `NaN`. Names and
dimensions are not supported; they are dropped and an error
respectively.

## See also

[fraction-arithmetic](https://pkg.mitchelloharawild.com/fraction/reference/fraction-arithmetic.md),
[fraction-comparison](https://pkg.mitchelloharawild.com/fraction/reference/fraction-comparison.md),
[`as_fraction()`](https://pkg.mitchelloharawild.com/fraction/reference/as_fraction.md),
[`numerator()`](https://pkg.mitchelloharawild.com/fraction/reference/numerator.md)

## Examples

``` r
fraction(1, 3) + fraction(1, 6)
#> <fraction<double>[1]>
#> [1] 1/2

# The fraction is kept as written
x <- fraction(c(2, 30), c(4, 100))
x
#> <fraction<double>[2]>
#> [1]    2/4 30/100
simplify(x)
#> <fraction<double>[2]>
#> [1]  1/2 3/10

# Stored as integers
fraction(1:3, 4L)
#> <fraction<integer>[3]>
#> [1] 1/4 2/4 3/4
```
