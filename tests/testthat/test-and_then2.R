test_that("and_then2 will fail with non-maybe values", {
  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b) {
      and_then2(a, b, list) %>% expect_error()
      and_then2(just(a), b, list) %>% expect_error()
      and_then2(a, just(b), list) %>% expect_error()
      and_then2(nothing(), b, list) %>% expect_error()
      and_then2(a, nothing(), list) %>% expect_error()
    }
  )
})

test_that("and_then2 will fail with a function which doesn't return a maybe value", {
  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b)
      and_then2(just(a), just(b), list) %>%
        expect_error()
  )
})

test_that("and_then2 will return nothing if any maybe is nothing", {
  and_then2(nothing(), nothing(), maybe(list)) %>%
    is_nothing() %>%
    expect_true()

  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b) {
      and_then2(nothing(), just(b), maybe(list)) %>%
        is_nothing() %>%
        expect_true()

      and_then2(just(a), nothing(), maybe(list)) %>%
        is_nothing() %>%
        expect_true()
    }
  )
})

test_that("and_then2 will unwrap maybes and apply a maybe returning function", {
  identity_list <- function(a, b) just(list(a, b))

  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b)
      and_then2(just(a), just(b), identity_list) %>%
        expect_identical(identity_list(a, b))
  )
})
