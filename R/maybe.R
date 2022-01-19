fmap <- function(.m, .f, ...) {
  UseMethod("fmap", .m)
}

bind <- function(.m, .f, ...) {
  UseMethod("bind", .m)
}

join <- function(.m, .f, ...) {
  UseMethod("join", .m)
}

#' @export
fmap.maybe <- function(.m, .f, ...) {
  if (is_just(.m))
    just(.f(.m$content, ...))

  else
    nothing()
}

#' @export
join.maybe <- function(.m) {
  if (is_just(.m) && is_maybe(.m$content))
    .m$content

  else
    .m
}

#' @export
bind.maybe <- function(.m, .f, ...) {
  join(fmap(.m, .f, ...))
}

#' @export
from_maybe <- function(.m, default) {
  if (is_just(.m))
    .m$content

  else
    default
}

#' @export
just <- function(a) {
  as_maybe(list(type = "just", content = a))
}

#' @export
nothing <- function() {
  as_maybe(list(type = "nothing"))
}

#' @export
maybe <- function(.f, allow_warning = FALSE, assert = \(a) TRUE) {
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
          \(a) !isTRUE(assert(a))

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

#' @export
map_maybe <- fmap

#' @export
and_then <- bind

#' @export
with_default <- from_maybe

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
