test_that("maybe_map3 will fail with non-maybe values", {
  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c) {
      maybe_map3(a, b, c, list) %>% expect_error()
      maybe_map3(just(a), b, c, list) %>% expect_error()
      maybe_map3(a, just(b), c, list) %>% expect_error()
      maybe_map3(a, b, just(c), list) %>% expect_error()
      maybe_map3(nothing(), b, c, list) %>% expect_error()
      maybe_map3(a, nothing(), c, list) %>% expect_error()
      maybe_map3(a, b, nothing(), list) %>% expect_error()
    }
  )
})

test_that("maybe_map3 will fail with a maybe returning function", {
  safe_list <- function(a, b, c) just(list(a, b, c))

  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c)
      maybe_map3(just(a), just(b), just(c), safe_list) %>%
        expect_error()
  )
})

test_that("maybe_map3 will return nothing if any maybe is nothing", {
  maybe_map3(nothing(), nothing(), nothing(), list) %>%
    is_nothing() %>%
    expect_true()

  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c) {
      maybe_map3(nothing(), just(b), just(c), list) %>%
        is_nothing() %>%
        expect_true()

      maybe_map3(just(a), nothing(), just(c), list) %>%
        is_nothing() %>%
        expect_true()

      maybe_map3(just(a), just(b), nothing(), list) %>%
        is_nothing() %>%
        expect_true()
    }
  )
})

test_that("maybe_map3 will unwrap maybes and rewrap the result", {
  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c)
      maybe_map3(just(a), just(b), just(c), list) %>%
        expect_identical(just(list(a, b, c)))
  )
})
