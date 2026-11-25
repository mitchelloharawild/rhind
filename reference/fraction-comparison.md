# Comparing fractions

Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`) are of the fraction's
value, so `2/4` equals `1/2`. Comparisons of fractions and whole numbers
are exact. Values are compared as doubles if any element of a plain
number isn't whole.

## Details

[`unique()`](https://rdrr.io/r/base/unique.html),
[`duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`match()`](https://rdrr.io/r/base/match.html),
[`sort()`](https://rdrr.io/r/base/sort.html),
[`order()`](https://rdrr.io/r/base/order.html) and grouping also use the
exact value. The first of equal fractions is kept as it was written.

## Examples

``` r
fraction(2, 4) == fraction(1, 2)
#> [1] TRUE
fraction(1, 3) < 0.5
#> [1] TRUE
unique(fraction(c(2, 1, 1), c(4, 2, 3)))
#> <fraction<double>[2]>
#> [1] 2/4 1/3
sort(fraction(c(2, 1, 5), c(3, 2, 8)))
#> <fraction<double>[3]>
#> [1] 1/2 5/8 2/3
```
