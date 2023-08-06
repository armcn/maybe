test_that("and_then3 will fail with non-maybe values", {
  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c) {
      and_then3(a, b, c, list) %>% expect_error()
      and_then3(just(a), b, c, list) %>% expect_error()
      and_then3(a, just(b), c, list) %>% expect_error()
      and_then3(a, b, just(c), list) %>% expect_error()
      and_then3(nothing(), b, c, list) %>% expect_error()
      and_then3(a, nothing(), c, list) %>% expect_error()
      and_then3(a, b, nothing(), list) %>% expect_error()
    }
  )
})

test_that("and_then3 will fail with a function which doesn't return a maybe value", {
  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c)
      and_then3(just(a), just(b), just(c), list) %>%
        expect_error()
  )
})

test_that("and_then3 will return nothing if any maybe is nothing", {
  and_then3(nothing(), nothing(), nothing(), maybe(list)) %>%
    is_nothing() %>%
    expect_true()

  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c) {
      and_then3(nothing(), just(b), just(c), maybe(list)) %>%
        is_nothing() %>%
        expect_true()

      and_then3(just(a), nothing(), just(c), maybe(list)) %>%
        is_nothing() %>%
        expect_true()

      and_then3(just(a), just(b), nothing(), maybe(list)) %>%
        is_nothing() %>%
        expect_true()
    }
  )
})

test_that("and_then3 will unwrap maybes and apply a maybe returning function", {
  identity_list <- function(a, b, c) just(list(a, b, c))

  for_all(
    a = anything(),
    b = anything(),
    c = anything(),
    property = function(a, b, c)
      and_then3(just(a), just(b), just(c), identity_list) %>%
        expect_identical(identity_list(a, b, c))
  )
})
