#' Check if a vector or data frame is empty
#'
#' @param a Object to check
#'
#' @examples
#' not_empty(integer())
#' not_empty(list())
#' not_empty(1:10)
#' not_empty(data.frame())
#' not_empty(data.frame(a = 1:10))
#' @return `TRUE` or `FALSE`
#' @export
not_empty <- function(a) {
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

#' Check if an object is not NULL
#'
#' @param a Object to check
#'
#' @examples
#' not_null(NULL)
#' not_null(1)
#' @return `TRUE` or `FALSE`
#' @export
not_null <- function(a) {
  Negate(is.null)(a)
}

#' Check if an object is not NA
#'
#' @param a Object to check
#'
#' @examples
#' not_na(NA)
#' not_na(1)
#' @return `TRUE` or `FALSE`
#' @export
not_na <- function(a) {
  not_true(is.na(a))
}

#' Check if an object is not NaN
#'
#' @param a Object to check
#'
#' @examples
#' not_nan(NaN)
#' not_nan(1)
#' @return `TRUE` or `FALSE`
#' @export
not_nan <- function(a) {
  not_true(and(is.atomic, is.nan)(a))
}

#' Check if an object is not infinite
#'
#' @param a Object to check
#'
#' @examples
#' not_infinite(Inf)
#' not_infinite(1)
#' @return `TRUE` or `FALSE`
#' @export
not_infinite <- function(a) {
  not_true(and(is.atomic, is.infinite)(a))
}

#' Check if an object is not undefined
#'
#' In this case 'undefined' values include `NULL`, `NaN`, all `NA` variants,
#' and infinite values.
#'
#' @param a Object to check
#'
#' @examples
#' not_undefined(NA)
#' not_undefined(NULL)
#' not_undefined(1)
#' @return `TRUE` or `FALSE`
#' @export
not_undefined <- function(a) {
  and(
    not_null,
    not_na,
    not_nan,
    not_infinite
  )(a)
}

#' Combine predicate functions to check if all are TRUE
#'
#' @param ... Predicate functions
#'
#' @examples
#' and(not_null, not_na)(1)
#' and(not_null, not_na)(NULL)
#' @return A predicate function
#' @export
and <- function(...) {
  funs <-
    list(...)

  \(a) {
    for (i in seq_along(funs))
      if (not_true(funs[[i]](a)))
        return(FALSE)

    TRUE
  }
}

#' Combine predicate functions to check if any are TRUE
#'
#' @param ... Predicate functions
#'
#' @examples
#' or(not_null, not_na)(1)
#' or(not_null, not_na)(NULL)
#' @return A predicate function
#' @export
or <- function(...) {
  funs <-
    list(...)

  \(a) {
    for (i in seq_along(funs))
      if (isTRUE(funs[[i]](a)))
        return(TRUE)

    FALSE
  }
}

not_true <- function(a) {
  Negate(isTRUE)(a)
}
