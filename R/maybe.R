#' Create a 'Just' variant of a maybe value
#'
#' @param a A value to wrap in a 'Just' container
#'
#' @examples
#' just(1)
#' just("hello")
#' @return A 'Just' variant of a maybe value
#' @export
just <- function(a) {
  as_maybe(list(type = "just", content = a))
}

#' Create a 'Nothing' variant of a maybe value
#'
#' @examples
#' nothing()
#' @return A 'Nothing' variant of a maybe value
#' @export
nothing <- function() {
  as_maybe(list(type = "nothing"))
}

#' Modify a function to return a maybe value
#'
#' Wrapping a function in `maybe` will modify it to return a maybe value. If
#' the function would normally return an error or warning the modified function
#' will return a 'Nothing' value, otherwise it will return a 'Just' value.
#' If a predicate function is provided with the parameter `ensure`, if the
#' predicate returns `TRUE` when evaluated on the return value of the function,
#' then a 'Just' value will be returned by the modified function, otherwise
#' it will return a 'Nothing' value.
#'
#' @param .f A function to modify
#' @param ensure A predicate function
#' @param allow_warning Whether warnings should result in 'Nothing' values
#'
#' @examples
#' maybe(mean)(1:10)
#' maybe(mean, allow_warning = TRUE)("hello")
#' maybe(sqrt)("hello")
#' maybe(sqrt, ensure = not_infinite)(-1)
#' @return A function which returns maybe values
#' @export
maybe <- function(.f, ensure = \(a) TRUE, allow_warning = FALSE) {
  \(...) {
    eval_f <-
      \(...) {
        result <-
          .f(...)

        if (not_true(ensure(result)))
          nothing()

        else
          just(result)
      }

    on_warning <-
      \(w)
        if (isTRUE(allow_warning))
          just(.f(...))

        else
          nothing()

    tryCatch(
      eval_f(...),
      error = \(e) nothing(),
      warning = on_warning
    )
  }
}

#' Modify a function to return the value or a default value
#'
#' Wrapping a function in `perhaps` will modify it to return the expected value
#' or a default value in some circumstances. If the function would normally
#' return an error or warning the modified function will return a default value,
#' otherwise it will return the expected value. If a predicate function is
#' provided with the parameter `ensure`, if the predicate returns `TRUE` when
#' evaluated on the return value of the function, then the expected value will
#' be returned by the modified function, otherwise it will return the default
#' value.
#'
#' @param .f A function to modify
#' @param default A default value
#' @param ensure A predicate function
#' @param allow_warning Whether warnings should result in the default value
#'
#' @examples
#' perhaps(mean, default = 0)(1:10)
#' perhaps(mean, default = 0, allow_warning = TRUE)("hello")
#' perhaps(sqrt, default = 0)("hello")
#' perhaps(sqrt, default = 0, ensure = not_infinite)(-1)
#' @return A function which returns the expected value or the default value
#' @export
perhaps <- function(.f, default, ensure = \(a) TRUE, allow_warning = FALSE) {
  \(...)
    maybe(.f, ensure = ensure, allow_warning = allow_warning)(...) |>
      with_default(default = default)
}

#' Evaluate a function on a maybe value
#'
#' @param .m A maybe value
#' @param .f A function to apply to the maybe value
#' @param ... Named arguments for the function `.f`
#'
#' @examples
#' just(9) |> maybe_map(sqrt)
#' nothing() |> maybe_map(sqrt)
#' @return A maybe value
#' @export
maybe_map <- function(.m, .f, ...) {
  assert_is_maybe(.m)

  if (is_just(.m))
    .f(.m$content, ...) |>
      assert_returns_not_maybe() |>
      just()

  else
    nothing()
}

#' @rdname maybe_map
#' @export
fmap <- maybe_map

#' Evaluate a maybe returning function on a maybe value
#'
#' @param .m A maybe value
#' @param .f A maybe returning function to apply to the maybe value
#' @param ... Named arguments for the function `.f`
#'
#' @examples
#' safe_sqrt <- maybe(sqrt, ensure = not_infinite)
#'
#' just(9) |> and_then(safe_sqrt)
#' just(-1) |> and_then(safe_sqrt)
#' nothing() |> and_then(safe_sqrt)
#' @return A maybe value
#' @export
and_then <- function(.m, .f, ...) {
  assert_is_maybe(.m)

  if (is_just(.m))
    .f(.m$content, ...) |>
      assert_returns_maybe()

  else
    nothing()
}

#' @rdname and_then
#' @export
bind <- and_then

#' Flatten a nested maybe value
#'
#' @param .m A maybe value
#'
#' @examples
#' just(just(1)) |> maybe_flatten()
#' just(nothing()) |> maybe_flatten()
#' just(1) |> maybe_flatten()
#' nothing() |> maybe_flatten()
#' @return A maybe value
#' @export
maybe_flatten <- function(.m) {
  UseMethod("maybe_flatten", .m)
}

#' @export
maybe_flatten.maybe <- function(.m) {
  if (is_just(.m) && is_maybe(.m$content))
    .m$content

  else
    .m
}

#' @rdname maybe_flatten
#' @export
join <- maybe_flatten

#' Unwrap a maybe value or return a default
#'
#' @param .m A maybe value
#' @param default A default value to return if the maybe value is 'Nothing'
#'
#' @examples
#' just(1) |> with_default(default = 0)
#' nothing() |> with_default(default = 0)
#' @return The unwrapped maybe value or the default value
#' @export
with_default <- function(.m, default) {
  UseMethod("with_default", .m)
}

#' @export
with_default.maybe <- function(.m, default) {
  if (is_just(.m))
    .m$content

  else
    default
}

#' @rdname with_default
#' @export
from_maybe <- with_default

#' Check if a maybe value contains a specific value
#'
#' If the maybe value is a 'Nothing' variant `FALSE` will be returned. If it is
#' a 'Just' variant the contents will be unwrapped and compared to the `value`
#' argument using `base::identical`.
#'
#' @param .m A maybe value
#' @param value A value to check
#'
#' @examples
#' just(1) |> maybe_contains(1)
#' just("a") |> maybe_contains(1)
#' nothing() |> maybe_contains(1)
#' @return `TRUE` or `FALSE`
#' @export
maybe_contains <- function(.m, value) {
  UseMethod("maybe_contains", .m)
}

#' @export
maybe_contains.maybe <- function(.m, value) {
  if (is_nothing(.m))
    FALSE

  else
    identical(.m$content, value)
}

#' Check if two maybe values are equal
#'
#' If both values are 'Nothing' variants or both values are 'Just' variants with
#' identical contents `TRUE` will be returned, otherwise `FALSE`.
#'
#' @param .m1 A maybe value
#' @param .m2 A maybe value
#'
#' @examples
#' maybe_equal(just(1), just(1))
#' maybe_equal(just(1), just(2))
#' maybe_equal(nothing(), nothing())
#' @return `TRUE` or `FALSE`
#' @export
maybe_equal <- function(.m1, .m2) {
  if (!is_maybe(.m1) || !is_maybe(.m2))
    stop("Both arguments must be maybe values")

  else
    identical(.m1, .m2)
}

#' Check if an object is a maybe value
#'
#' @param a Object to check
#'
#' @examples
#' is_maybe(1)
#' is_maybe(just(1))
#' is_maybe(nothing())
#' @return `TRUE` or `FALSE`
#' @export
is_maybe <- function(a) {
  identical(class(a), "maybe")
}

#' Check if an object is a 'Just' value
#'
#' @param a Object to check
#'
#' @examples
#' is_just(1)
#' is_just(just(1))
#' is_just(nothing())
#' @return `TRUE` or `FALSE`
#' @export
is_just <- function(a) {
  and(is_maybe, \(b) identical(b$type, "just"))(a)
}

#' Check if an object is a 'Nothing' value
#'
#' @param a Object to check
#'
#' @examples
#' is_nothing(1)
#' is_nothing(just(1))
#' is_nothing(nothing())
#' @return `TRUE` or `FALSE`
#' @export
is_nothing <- function(a) {
  and(is_maybe, \(b) identical(b$type, "nothing"))(a)
}

#' @export
print.maybe <- function(x, ...) {
  if (is_just(x)) {
    cat("Just\n")
    print(x$content, ...)

  } else
    cat("Nothing")
}

as_maybe <- function(a) {
  structure(a, class = "maybe")
}

assert_is_maybe <- function(a) {
  if (is_maybe(a))
    invisible(a)

  else
    stop("The argument '.m' must be a maybe value.", call. = FALSE)
}

assert_returns_not_maybe <- function(a) {
  if (is_maybe(a))
    stop(
      "The function provided to 'maybe_map' must not return a maybe value.",
      call. = FALSE
    )

  else
    invisible(a)
}

assert_returns_maybe <- function(a) {
  if (is_maybe(a))
    invisible(a)

  else
    stop(
      "The function provided to 'and_then' must return a maybe value.",
      call. = FALSE
    )
}
