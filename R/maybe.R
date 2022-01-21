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
#' @export
maybe <- function(.f, allow_warning = FALSE, ensure = \() TRUE) {
  \(...) {
    on_warning <-
      \(w)
        if (allow_warning)
          .f(...)

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

#' Modify a function to return a default value
#'
#' @export
perhaps <- function(.f,
                    otherwise,
                    allow_warning = FALSE,
                    ensure = \() TRUE) {
  \(...)
    maybe(.f, allow_warning = allow_warning, ensure = ensure)(...) |>
      with_default(default = otherwise)
}

#' Evaluate a function on a maybe value
#'
#' @export
map_maybe <- function(.m, .f, ...) {
  UseMethod("map_maybe", .m)
}

#' Evaluate a maybe producing function on a maybe value
#'
#' @export
and_then <- function(.m, .f, ...) {
  UseMethod("and_then", .m)
}

#' Flatten a nested maybe value
#'
#' @export
flatten_maybe <- function(.m, .f, ...) {
  UseMethod("flatten_maybe", .m)
}

#' @export
map_maybe.maybe <- function(.m, .f, ...) {
  if (is_just(.m))
    just(.f(.m$content, ...))

  else
    nothing()
}

#' @export
flatten_maybe.maybe <- function(.m) {
  if (is_just(.m) && is_maybe(.m$content))
    .m$content

  else
    .m
}

#' @export
and_then.maybe <- function(.m, .f, ...) {
  flatten_maybe(map_maybe(.m, .f, ...))
}

#' Unwrap a maybe value and return a default for 'Nothing'
#'
#' @export
with_default <- function(.m, default) {
  if (is_just(.m))
    .m$content

  else
    default
}

#' @rdname map_maybe
#' @export
fmap <- map_maybe

#' @rdname flatten_maybe
#' @export
join <- flatten_maybe

#' @rdname and_then
#' @export
bind <- and_then

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
