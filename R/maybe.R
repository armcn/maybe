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
maybe <- function(.f, allow_warning = FALSE) {
  \(...) {
    on_warning <-
      \(w)
        if (allow_warning)
          .f(...)

        else
          nothing()

    on_error <-
      \(e) nothing()

    tryCatch(
      just(.f(...)),
      error = on_error,
      warning = on_warning
    )
  }
}

#' @export
print.maybe <- function(x, ...) {
  if (x$type == "just") {
    cat("Just\n")
    print(x$content, ...)

  } else {
    cat("Nothing")
  }
}

#' @export
maybe_map <- fmap

#' @export
and_then <- bind

#' @export
with_default <- from_maybe

as_maybe <- function(a) {
  structure(a, class = "maybe")
}

is_maybe <- function(.m) {
  class(.m) == "maybe"
}

is_just <- function(.m) {
  is_maybe(.m) && .m$type == "just"
}

is_nothing <- function(.m) {
  is_maybe(.m) && .m$type == "nothing"
}
