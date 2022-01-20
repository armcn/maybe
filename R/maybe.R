#' @export
just <- function(a) {
  as_maybe(list(type = "just", content = a))
}

#' @export
nothing <- function() {
  as_maybe(list(type = "nothing"))
}

#' @export
maybe <- function(.f, allow_warning = FALSE, result = not_undefined) {
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
        res <-
          .f(...)

        assertion_failed <-
          \(a) !isTRUE(result(a))

        if (assertion_failed(res))
          nothing()

        else
          just(res)
      }

    tryCatch(
      eval_f(...),
      error = on_error,
      warning = on_warning
    )
  }
}

#' @export
perhaps <- function(.f,
                    otherwise,
                    allow_warning = FALSE,
                    result = not_undefined) {
  \(...)
    maybe(.f, allow_warning = allow_warning, result = result)(...) |>
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

#' @rdname
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
