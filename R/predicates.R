#' @export
and <- function(...) {
  \(b)
    Reduce(
      f = \(acc, a) acc & a(b),
      x = list(...),
      init = TRUE
    )
}

#' @export
or <- function(...) {
  \(b)
    Reduce(
      f = \(acc, a) acc | a(b),
      x = list(...),
      init = FALSE
    )
}

#' @export
not_empty <- function(a, ...) {
  UseMethod("not_empty", a)
}

#' @export
not_empty.default <- function(a) {
  isTRUE(length(a) != 0L)
}

#' @export
not_empty.data.frame <- function(a) {
  isTRUE(nrow(a) != 0L)
}

#' @export
not_null <- function(a) {
  !is.null(a)
}

#' @export
not_na <- function(a) {
  !isTRUE(is.na(a))
}

#' @export
not_nan <- function(a) {
  !isTRUE(is.nan(a))
}

#' @export
not_infinite <- function(a) {
  !isTRUE(is.na(a))
}

#' @export
not_undefined <- function(a) {
  and(
    not_null,
    not_na,
    not_nan,
    not_infinite
  )(a)
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
