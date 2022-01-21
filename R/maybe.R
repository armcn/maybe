#' Create a 'Just' variant of a maybe value
#'
#' @param a A value to wrap in a 'Just'
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
#' predicate returns `FALSE` when called on the return value of the function,
#' then a 'Nothing' value will be returned by the modified function, otherwise
#' it will return a 'Just' value.
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
#' provided with the parameter `ensure`, if the predicate returns `FALSE` when
#' called on the return value of the function, then a default value will be
#' returned by the modified function, otherwise it will return the expected
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
#' just(9) |> map_maybe(sqrt)
#' nothing() |> map_maybe(sqrt)
#' @return A maybe value
#' @export
map_maybe <- function(.m, .f, ...) {
  UseMethod("map_maybe", .m)
}

#' @export
map_maybe.maybe <- function(.m, .f, ...) {
  if (is_just(.m))
    just(.f(.m$content, ...))

  else
    nothing()
}

#' @rdname map_maybe
#' @export
fmap <- map_maybe

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
  UseMethod("and_then", .m)
}

#' @export
and_then.maybe <- function(.m, .f, ...) {
  flatten_maybe(map_maybe(.m, .f, ...))
}

#' @rdname and_then
#' @export
bind <- and_then

#' Flatten a nested maybe value
#'
#' @param .m A maybe value
#'
#' @examples
#' just(just(1)) |> flatten_maybe()
#' just(nothing()) |> flatten_maybe()
#' just(1) |> flatten_maybe()
#' nothing() |> flatten_maybe()
#' @return A maybe value
#' @export
flatten_maybe <- function(.m) {
  UseMethod("flatten_maybe", .m)
}

#' @export
flatten_maybe.maybe <- function(.m) {
  if (is_just(.m) && is_maybe(.m$content))
    .m$content

  else
    .m
}

#' @rdname flatten_maybe
#' @export
join <- flatten_maybe

#' Unwrap a maybe value and return a default for 'Nothing'
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

#' @export
print.maybe <- function(x, ...) {
  if (x$type == "just") {
    cat("Just\n")
    print(x$content, ...)

  } else
    cat("Nothing")
}

as_maybe <- function(a) {
  structure(a, class = "maybe")
}
