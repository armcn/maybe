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
#' @param allow_warning Whether warnings should result in 'Nothing' values
#' @param ensure A predicate function
#'
#' @examples
#' maybe(mean)(1:10)
#' maybe(mean, allow_warning = FALSE)("hello")
#' maybe(sqrt)("hello")
#' maybe(sqrt, ensure = not_infinite)(-1)
#' @return A function which returns maybe values
#' @export
maybe <- function(.f, allow_warning = FALSE, ensure = \(a) TRUE) {
  \(...) {
    on_warning <-
      \(w)
        if (allow_warning)
          just(.f(...))

        else
          nothing()

    on_error <-
      \(e) nothing()

    eval_f <-
      \(...) {
        result <-
          .f(...)

        assertion_failed <-
          \(a) !isTRUE(ensure(a))

        if (assertion_failed(result))
          nothing()

        else
          just(result)
      }

    tryCatch(
      eval_f(...),
      error = on_error,
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
#' @param allow_warning Whether warnings should result in the default value
#' @param ensure A predicate function
#'
#' @examples
#' perhaps(mean, default = 0)(1:10)
#' perhaps(mean, default = 0, allow_warning = TRUE)("hello")
#' perhaps(sqrt, default = 0)("hello")
#' perhaps(sqrt, default = 0, ensure = not_infinite)(-1)
#' @return A function which returns the expected value or the default value
#' @export
perhaps <- function(.f,
                    default,
                    allow_warning = FALSE,
                    ensure = \(a) TRUE) {
  \(...)
    maybe(.f, allow_warning = allow_warning, ensure = ensure)(...) |>
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
map_maybe.maybe <- function(.m, .f, ...) {
  if (is_just(.m))
    just(.f(.m$content, ...))

  else
    nothing()
}

#' @rdname map_maybe
#' @export
fmap <- map_maybe

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

#' @export
and_then.maybe <- function(.m, .f, ...) {
  flatten_maybe(map_maybe(.m, .f, ...))
}

#' @rdname and_then
#' @export
bind <- and_then

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

  } else {
    cat("Nothing")
  }
}

as_maybe <- function(a) {
  structure(a, class = "maybe")
}
