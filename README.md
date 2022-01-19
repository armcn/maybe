
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maybe <img src="man/figures/hex.png" align="right" style="width: 20%;"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/maybe)](https://CRAN.R-project.org/package=maybe)
[![R-CMD-check](https://github.com/armcn/maybe/workflows/R-CMD-check/badge.svg)](https://github.com/armcn/maybe/actions)
[![Codecov test
coverage](https://codecov.io/gh/armcn/maybe/branch/main/graph/badge.svg)](https://codecov.io/gh/armcn/maybe?branch=main)
<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("armcn/maybe")
```

# Usage

Maybe values can be used to model computations that may fail or have
undefined outputs. For example, dividing by zero is mathematically
undefined but many programming languages, including R, return infinity.
This can cause unexpected problems later on in the program. We can use a
maybe value to make a safe divide function.

``` r
1 / 0
#> [1] Inf
```

``` r
library(maybe)

safe_divide <- function(a, b) {
  if (b == 0) nothing() else just(a / b)
}

safe_divide(2, 2)
#> Just
#> [1] 1
safe_divide(2, 0)
#> Nothing
```

This time `2 / 2` returns `Just 1` and `2 / 0` returns `Nothing`. These
are the two possibly values of a maybe value. It can be `Just` the
value, or it can be `Nothing` (the absence of a value). In order to use
the value for another computation, first you need to specify what will
happen if the function returns `Nothing`. This can be done using the
`with_default` function. This function will return the value contained
in the `Just`, or if it is `Nothing` it will return the default.

``` r
safe_divide(2, 2) |> with_default(0)
#> [1] 1
safe_divide(2, 0) |> with_default(0)
#> [1] 0
```

It gets more interesting when we chain maybe values together. Instead of
having to use `with_default` every time you use a function that produces
maybe values, they can be chained and `with_default` only needs to be
called once at the end.

For example if we divide two numbers then get the absolute value. This
is accomplished with the `map_maybe` function.

``` r
safe_divide(-2, 2) |> map_maybe(abs) |> with_default(0)
#> [1] 1
safe_divide(-2, 0) |> map_maybe(abs) |> with_default(0)
#> [1] 0
```

What if we wanted to chain multiple “safe” functions (functions that
return maybe values) together? For that we use `and_then`.

First let’s define two “safe” functions then call them one after the
other.

``` r
safe_mean <- function(a) {
  if (length(a) == 0L) nothing() else just(mean(a))
}

safe_sqrt <- function(a) {
  if (a == -1) nothing() else just(sqrt(a))
}

seq(1, 10) |> safe_mean() |> and_then(safe_sqrt) |> with_default(0)
#> [1] 2.345208
```

Functions can also be converted to “safe” functions in another way.
Instead of rewriting the function we can wrap it in `maybe`. This will
alter the function to return `Nothing` on error or warning and we can
optionally provide a predicate function (a function that returns TRUE or
FALSE) to assert something about the output. For example here are the
above functions made “safe” with `maybe`.

``` r
safe_mean <- maybe(mean, assert = not_nan)

safe_sqrt <- maybe(sqrt, assert = not_infinite)

seq(1, 10) |> safe_mean() |> and_then(safe_sqrt) |> with_default(0)
#> [1] 2.345208

maybe(mean)("hello")
#> Nothing
```

Combine multiple predicates with `and` or `or` functions.

``` r
safe_mean <- maybe(mean, assert = and(not_nan, not_na))

safe_mean(1:10)
#> Just
#> [1] 5.5
```

Or use predefined combinations like `not_undefined` which checks if the
output is not `NULL`, `NA`, `NaN`, or infinte.

``` r
safe_mean <- maybe(mean, assert = not_undefined)

safe_mean(c(NA, 1, 2))
#> Nothing
```
