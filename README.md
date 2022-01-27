
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maybe <img src="man/figures/hex.png" align="right" style="width: 25%;"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/maybe)](https://CRAN.R-project.org/package=maybe)
[![R-CMD-check](https://github.com/armcn/maybe/workflows/R-CMD-check/badge.svg)](https://github.com/armcn/maybe/actions)
[![Codecov test
coverage](https://codecov.io/gh/armcn/maybe/branch/main/graph/badge.svg)](https://app.codecov.io/gh/armcn/maybe?branch=main)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/maybe)](https://cran.r-project.org/package=maybe)

<!-- badges: end -->

# Overview

The maybe type represents the possibility of some value or nothing. It
is often used instead of throwing an error or returning an undefined
value like `NA` or `NULL`. The advantage of using a maybe type is that
the functions which work with it are both composable and require the
developer to explicitly acknowledge the potential absence of a value,
helping to avoid unexpected behavior.

## Installation

You can install the released version of maybe from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("maybe")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("armcn/maybe")
```

# Usage

Maybe values can be used to model computations that may fail or have
undefined outputs. For example, dividing by zero is mathematically
undefined but in many programming languages, including R, infinity is
returned. If it is not properly accounted for this may cause unexpected
behavior later in the program. The maybe type can be used to improve the
safety of the divide function.

``` r
library(maybe)

`%//%` <- function(a, b) {
  if (b == 0) nothing() else just(a / b)
}

10 / 2
#> [1] 5
10 %//% 2
#> Just
#> [1] 5

10 / 0
#> [1] Inf
10 %//% 0
#> Nothing
```

`10 %//% 2` returns `Just 5` and `10 %//% 0` returns `Nothing`. These
are the two possibly values of the maybe type. It can be `Just` the
value, or it can be `Nothing`, the absence of a value. For the value to
be used as an input of another function you need to specify what will
happen if the function returns `Nothing`. This can be done using the
`with_default` function. This function will return the value contained
in the `Just`, or if it is `Nothing` it will return the default. Think
of a maybe value as a container. In this container can be `Just` the
value or `Nothing`. To use the contained value in a regular R function
you need to unwrap it first.

``` r
10 %//% 2
#> Just
#> [1] 5
10 %//% 2 |> with_default(0)
#> [1] 5

10 %//% 0
#> Nothing
10 %//% 0 |> with_default(0)
#> [1] 0
```

This may seem tedious to rewrite functions to return maybe values and
then specify a default value each time. This is where the maybe chaining
functions become useful. `map_maybe` allows a regular R function to be
evaluated on a maybe value. `map_maybe`, often called `fmap` in other
languages, reaches into the maybe value, applies a function to the
value, then re-wraps the result in a maybe. If the input is a `Just`
value, the return value of `map_maybe` will also be a `Just`. If it is
`Nothing` the return value will be `Nothing`.

``` r
safe_max <- function(a) {
  if (length(a) == 0L) nothing() else just(max(a))
}

just(9) |> map_maybe(sqrt)
#> Just
#> [1] 3
nothing() |> map_maybe(sqrt)
#> Nothing

safe_max(1:9) |> map_maybe(sqrt)
#> Just
#> [1] 3
safe_max(integer()) |> map_maybe(sqrt)
#> Nothing
```

What if we wanted to chain multiple “safe” functions (functions that
return maybe values) together? `and_then`, often called `bind` in other
languages, works similarly to `map_maybe` except the function provided
must return a maybe value.

``` r
safe_sqrt <- function(a) {
  if (a < 0) nothing() else just(sqrt(a))
}

just(9) |> and_then(safe_sqrt)
#> Just
#> [1] 3
nothing() |> and_then(safe_sqrt)
#> Nothing

safe_max(1:9) |> and_then(safe_sqrt)
#> Just
#> [1] 3
safe_max(integer()) |> and_then(safe_sqrt)
#> Nothing
```

The maybe package provides another way to create functions that return
maybe values. Instead of rewriting the function to return maybe values
we can wrap it in the `maybe` function. This will modify the function to
return `Nothing` on an error or warning. A predicate function (a
function that returns `TRUE` or `FALSE`) can be provided as an argument
to assert something about the return value. If the predicate returns
`FALSE` then a `Nothing` value will be returned, otherwise it will be a
`Just` value.

``` r
safe_max <- maybe(max)
safe_sqrt <- maybe(sqrt, ensure = not_infinite)

safe_max(1:9) |> and_then(safe_sqrt)
#> Just
#> [1] 3
safe_max("hello") |> and_then(safe_sqrt)
#> Nothing
```

This pattern of modifying a function with the `maybe` function and then
setting a default value is so common that there is a shortcut,
`perhaps`. The default value is set with the `default` parameter. This
function will always return a regular R value, never maybe values.

``` r
safe_max <- perhaps(max, ensure = is.numeric, default = 0)

safe_max(1:9) |> sqrt()
#> [1] 3
safe_max("hello") |> sqrt()
#> [1] 0
```

Multiple predicates can be combined with the `and`/`or` functions.

``` r
safe_sqrt <- maybe(mean, ensure = and(not_infinite, not_empty))

safe_sqrt(9)
#> Just
#> [1] 9
```

Predefined combinations are also provided such as `not_undefined`, which
ensures that the output is not any of `NULL`, `NA`, `NaN`, `-Inf`, or
`Inf`.

``` r
safe_mean <- maybe(mean, ensure = not_undefined)

safe_mean(c(1, 2, 3))
#> Just
#> [1] 2
safe_mean(c(NA, 2, 3))
#> Nothing
```

## Function names

The names of functions `map_maybe`, `and_then`, `flatten_maybe`, and
`with_default` are different from the traditional names used for these
functions in other functional programming languages. If you would like
to use the more traditional names aliases are provided.

-   `fmap` == `map_maybe`
-   `bind` == `and_then`
-   `join` == `flatten_maybe`
-   `from_maybe` == `with_default`

## Inspiration / Prior work

-   [monads R package](https://github.com/hadley/monads)
-   [rmonad R package](https://github.com/arendsee/rmonad)
-   [Maybe Monad in R blog
    post](https://www.r-bloggers.com/2019/05/maybe-monad-in-r/)
-   [Elm Maybe
    package](https://package.elm-lang.org/packages/elm/core/1.0.5/Maybe)
