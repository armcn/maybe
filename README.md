
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
library(maybe)

safe_divide <- function(a, b) {
  if (b == 0) nothing() else just(a / b)
}

2 / 2
#> [1] 1
safe_divide(2, 2)
#> Just
#> [1] 1

2 / 0
#> [1] Inf
safe_divide(2, 0)
#> Nothing
```

This time `2 / 2` returns `Just 1` and `2 / 0` returns `Nothing`. These
are the two possibly values of a maybe value. It can be `Just` the
value, or it can be `Nothing` (the absence of a value). For the maybe
value to be used by another function, first you need to specify what
will happen if the function returns `Nothing`. This can be done using
the `with_default` function. This function will return the value
contained in the `Just`, or if it is `Nothing` it will return the
default. Think of a maybe value as a container. In this container can be
`Just` the value or `Nothing`. To use the contained value in regular R
functions you need to unwrap it first.

``` r
safe_divide(2, 2)
#> Just
#> [1] 1
safe_divide(2, 2) |> with_default(0)
#> [1] 1

safe_divide(2, 0)
#> Nothing
safe_divide(2, 0) |> with_default(0)
#> [1] 0
```

It gets more interesting when we chain maybe values together. Instead of
having to use `with_default` every time you use a function that produces
maybe values, they can be chained and `with_default` only needs to be
called once at the end.

As an example, we may want to divide two numbers then get the absolute
value of the result. This is accomplished with the `map_maybe` function.
`map_maybe`, often called `fmap` in other languages, reaches into the
maybe value, applies a function to the value, then re-wraps the result
in a maybe. If the input is a `Just` value, the return value of
`map_maybe` will also be a `Just`. If it is `Nothing` the return value
will be `Nothing`.

``` r
safe_divide(-2, 2)
#> Just
#> [1] -1
safe_divide(-2, 2) |> map_maybe(abs)
#> Just
#> [1] 1

safe_divide(-2, 0)
#> Nothing
safe_divide(-2, 0) |> map_maybe(abs)
#> Nothing
```

What if we wanted to chain multiple “safe” functions (functions that
return maybe values) together? For that we use `and_then`. This function
is often called `bind` in other languages. First let’s define two “safe”
functions then call them one after the other.

``` r
safe_mean <- function(a) {
  if (length(a) == 0L) nothing() else just(mean(a))
}

safe_sqrt <- function(a) {
  if (a == -1) nothing() else just(sqrt(a))
}

safe_mean(1:10) |> and_then(safe_sqrt)
#> Just
#> [1] 2.345208
```

Functions can also be converted to “safe” functions in another way.
Instead of rewriting the function we can wrap it in the function
`maybe`. This will modify the function to return `Nothing` on error or
warning and we can optionally provide a predicate function (a function
that returns `TRUE` or `FALSE`) to assert something about the result.
For example here are the above functions made “safe” with the `maybe`
function.

``` r
safe_mean <- maybe(mean, result = not_nan)
safe_sqrt <- maybe(sqrt, result = not_infinite)

safe_mean("hello") |> and_then(safe_sqrt)
#> Nothing
safe_mean(1:10) |> and_then(safe_sqrt)
#> Just
#> [1] 2.345208
```

It will automatically capture errors and warnings and return `Nothing`.
Warnings can be allowed by setting `allow_warnings = TRUE`.

``` r
safe_mean <- maybe(mean)

safe_mean(1:10)
#> Just
#> [1] 5.5
safe_mean("hello")
#> Nothing

safe_mean <- maybe(mean, allow_warning = TRUE)

safe_mean("hello")
#> Warning in mean.default(...): argument is not numeric or logical: returning NA
#> [1] NA
```

The pattern of wrapping a function in the `maybe` function and then
setting a default value is so common there is a shortcut, `perhaps`. The
default value is set with the `otherwise` parameter. This function will
always return regular R values, not maybes.

``` r
safe_mean <- perhaps(mean, otherwise = 0)

safe_mean(1:10)
#> [1] 5.5
safe_mean("hello")
#> [1] 0
```

We can combine multiple predicates with the `and`/`or` functions.

``` r
safe_mean <- maybe(mean, result = and(not_nan, not_empty))

safe_mean(1:10)
#> Just
#> [1] 5.5
```

Or use predefined combinations like `not_undefined` which checks if the
output is not any of `NULL`, `NA`, `NaN`, `-Inf`, or `Inf`.

``` r
safe_mean <- maybe(mean, result = not_undefined)

safe_mean(c(1, 2, 3))
#> Just
#> [1] 2
safe_mean(c(NA, 2, 3))
#> Nothing
```

The names of `map_maybe`, `and_then`, `flatten_maybe`, and
`with_default` are different than the traditional function names in
other FP languages. I believe these names more clearly articulate the
purpose of these functions but aliases are provided. In a single project
please choose to use one set or the other, not both.

-   `fmap` == `map_maybe`
-   `join` == `flatten_maybe`
-   `bind` == `and_then`
-   `from_maybe` == `with_default`
