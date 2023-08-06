test_that("maybe_map2 will fail with non-maybe values", {
  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b) {
      maybe_map2(a, b, list) %>% expect_error()
      maybe_map2(just(a), b, list) %>% expect_error()
      maybe_map2(a, just(b), list) %>% expect_error()
      maybe_map2(nothing(), b, list) %>% expect_error()
      maybe_map2(a, nothing(), list) %>% expect_error()
    }
  )
})

test_that("maybe_map2 will fail with a maybe returning function", {
  safe_list <- function(a, b) just(list(a, b))

  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b)
      maybe_map2(just(a), just(b), safe_list) %>%
        expect_error()
  )
})

test_that("maybe_map2 will return nothing if either maybe is nothing", {
  maybe_map2(nothing(), nothing(), list) %>%
    is_nothing() %>%
    expect_true()

  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b) {
      maybe_map2(nothing(), just(b), list) %>%
        is_nothing() %>%
        expect_true()

      maybe_map2(just(a), nothing(), list) %>%
        is_nothing() %>%
        expect_true()
    }
  )
})

test_that("maybe_map2 will unwrap maybes and rewrap the result", {
  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b)
      maybe_map2(just(a), just(b), list) %>%
        expect_identical(just(list(a, b)))
  )
})
