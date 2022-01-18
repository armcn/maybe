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
maybe <- function(.f,
                  allow_warning = FALSE,
                  allow_empty_vector = FALSE,
                  allow_empty_dataframe = FALSE,
                  assert = \(a) TRUE) {
  \(...) {
    on_warning <-
      \(w)
        if (allow_warning)
          .f(...)

        else
          nothing()

    on_error <-
      \(e) nothing()

    is_empty_vector <-
      \(a) length(a) == 0L

    is_empty_dataframe <-
      \(a) is.data.frame(a) && nrow(a) == 0L

    is_disallowed_empty_vector <-
      \(a)
        !allow_empty_vector &&
        is_empty_vector(a)

    is_disallowed_empty_dataframe <-
      \(a)
        !allow_empty_dataframe &&
        is_empty_dataframe(a)

    eval_f <-
      \(...) {
        result <-
          .f(...)

        assertion_failed <-
          \(a) !isTRUE(assert(a))

        if (is.null(result) ||
            is_disallowed_empty_vector(result) ||
            is_disallowed_empty_dataframe(result) ||
            assertion_failed(result))
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

is_maybe <- function(.m) {
  class(.m) == "maybe"
}

is_just <- function(.m) {
  is_maybe(.m) && .m$type == "just"
}

is_nothing <- function(.m) {
  is_maybe(.m) && .m$type == "nothing"
}

