# Exact fractions

``` r

library(rhind)
```

## The problem

Most fractions can’t be stored exactly as a number, so arithmetic with
them is approximate.

``` r

1 / 10 + 2 / 10 == 3 / 10
#> [1] FALSE

fraction(1, 10) + fraction(2, 10) == fraction(3, 10)
#> [1] TRUE
```

A number also forgets how it was written: 3 out of 10 and 30 out of 100
are both `0.3`. The rhind package stores the numerator `n` and
denominator `d` separately, which keeps both the exact value and the
terms it was written in.

## Data structure

A `fraction` is a vctrs record with two fields. It works like any other
vector, including in data frames.

![Diagram of a record with a numerator field n and a denominator field
d, printed as written.](figures/rcrd.svg)

Diagram of a record with a numerator field n and a denominator field d,
printed as written.

``` r

x <- fraction(c(2, 1, -5, NA), c(4, 3, 1, 1))
x
#> <fraction<double>[4]>
#> [1] 2/4 1/3  -5  NA
frac_num(x)
#> [1]  2  1 -5 NA
frac_den(x)
#> [1]  4  3  1 NA
```

A denominator of zero is allowed: `1/0`, `-1/0` and `0/0` behave as
`Inf`, `-Inf` and `NaN`.

## Converting

[`as_fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/as_fraction.md)
reads text, and approximates numbers which aren’t whole.

``` r

as_fraction(c("2/4", "3", "1 3/4"))
#> <fraction<integer>[3]>
#> [1] 2/4   3 7/4
as_fraction(c(0.1, 0.75, 1 / 3))
#> <fraction<double>[3]>
#> [1] 1/10  3/4  1/3
as_fraction(pi, max_d = 1000)
#> <fraction<double>[1]>
#> [1] 355/113
as.double(fraction(1, 3))
#> [1] 0.3333333
```

## Arithmetic

Fractions are kept as written, but results are in lowest terms.
[`frac_simplify()`](https://pkg.mitchelloharawild.com/rhind/reference/frac_num.md)
reduces a fraction without calculating anything.

![Diagram of 1/3 + 1/6: terms are cross multiplied to 9/18, then reduced
to 1/2.](figures/add.svg)

Diagram of 1/3 + 1/6: terms are cross multiplied to 9/18, then reduced
to 1/2.

``` r

fraction(1, 3) + fraction(1, 6)
#> <fraction<double>[1]>
#> [1] 1/2
fraction(2, 3)^-2
#> <fraction<double>[1]>
#> [1] 9/4
frac_simplify(fraction(2, 4))
#> <fraction<double>[1]>
#> [1] 1/2
sum(fraction(1, 1:10))
#> <fraction<double>[1]>
#> [1] 7381/2520
mean(fraction(1:3, 4))
#> <fraction<double>[1]>
#> [1] 1/2
round(fraction(2, 3), 2)
#> <fraction<double>[1]>
#> [1] 67/100
```

Functions with results that usually aren’t fractions, such as
[`sqrt()`](https://rdrr.io/r/base/MathFun.html) and
[`exp()`](https://rdrr.io/r/base/Log.html), give doubles.

## Rewriting

[`frac_rescale()`](https://pkg.mitchelloharawild.com/rhind/reference/frac_rescale.md)
and
[`frac_align()`](https://pkg.mitchelloharawild.com/rhind/reference/frac_rescale.md)
change the denominator without changing the value, and
[`frac_approx()`](https://pkg.mitchelloharawild.com/rhind/reference/frac_approx.md)
finds the closest fraction with a smaller denominator.

``` r

frac_rescale(fraction(3, 4), 100)
#> <fraction<double>[1]>
#> [1] 75/100
frac_align(fraction(1, c(2, 3)))
#> <fraction<double>[2]>
#> [1] 3/6 2/6
frac_approx(fraction(355, 113), 10)
#> <fraction<double>[1]>
#> [1] 22/7
```

[`frac_pool()`](https://pkg.mitchelloharawild.com/rhind/reference/frac_pool.md)
adds the numerators and denominators as they were written, unlike
[`mean()`](https://rdrr.io/r/base/mean.html) which weights all fractions
equally.

``` r

x <- fraction(c(3, 30, 1), c(10, 100, 2))
frac_pool(x)
#> <fraction<double>[1]>
#> [1] 34/112
mean(x)
#> <fraction<double>[1]>
#> [1] 11/30
```

## Plain numbers

Whole numbers are fractions. If any element of a plain number isn’t
whole, the result is a double, as it is when adding an integer to a
double. Convert with
[`as_fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/as_fraction.md)
first to remain exact.

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

The type made by combining vectors can’t depend on their values, so
combining with any double gives doubles. Use integers (such as `1L`) or
[`as_fraction()`](https://pkg.mitchelloharawild.com/rhind/reference/as_fraction.md)
to combine as fractions.

``` r

c(fraction(1, 3), 1)
#> [1] 0.3333333 1.0000000
c(fraction(1, 3), 1L)
#> <fraction<double>[2]>
#> [1] 1/3   1
```

## Comparison

Comparisons use the exact value, regardless of the terms. So do sorting,
matching and grouping.

``` r

fraction(2, 4) == fraction(1, 2)
#> [1] TRUE
unique(fraction(c(2, 1, 1), c(4, 2, 3)))
#> <fraction<double>[2]>
#> [1] 2/4 1/3
sort(fraction(c(2, 1, 5), c(3, 2, 8)))
#> <fraction<double>[3]>
#> [1] 1/2 5/8 2/3
```

## Storage

The fields are integers if both inputs are integers, and otherwise
doubles. Note that `1` is a double and `1L` is an integer. Integers
become doubles when a result doesn’t fit, and results too large for
doubles are missing.

![Diagram of integer storage becoming double, and double becoming
missing, when results are too large.](figures/storage.svg)

Diagram of integer storage becoming double, and double becoming missing,
when results are too large.

``` r

x <- fraction(.Machine$integer.max, 2L)
x
#> <fraction<integer>[1]>
#> [1] 2147483647/2
x * 3L
#> <fraction<double>[1]>
#> [1] 6442450941/2
fraction(1, 2^53) / 3
#> Warning: NAs produced by fractions too large to store exactly
#> <fraction<double>[1]>
#> [1] NA
```

Denominators grow quickly when adding fractions with many different
denominators. Use [`as.double()`](https://rdrr.io/r/base/double.html)
for approximate results of any size.

``` r

sum(fraction(1, 1:40))
#> <fraction<double>[1]>
#> [1] 2078178381193813/485721041551200
sum(as.double(fraction(1, 1:100)))
#> [1] 5.187378
```
