test_that("maybe_map2 will fail with non-maybe values", {
  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b) {
      maybe_map2(a, b, identity) %>% expect_error()
      maybe_map2(just(a), b, identity) %>% expect_error()
      maybe_map2(a, just(b), identity) %>% expect_error()
      maybe_map2(nothing(), b, identity) %>% expect_error()
      maybe_map2(a, nothing(), identity) %>% expect_error()
    }
  )
})

test_that("maybe_map2 will fail with a maybe returning function", {
  safe_identity_list <- function(a, b) just(list(a, b))

  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b)
      maybe_map2(just(a), just(b), safe_identity_list) %>%
        expect_error()
  )
})

test_that("maybe_map2 will return nothing if either maybe is nothing", {
  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b) {
      maybe_map2(nothing(), just(b), identity) %>%
        is_nothing() %>%
        expect_true()

      maybe_map2(just(a), nothing(), identity) %>%
        is_nothing() %>%
        expect_true()
    }
  )
})

test_that("maybe_map2 will unwrap maybes and rewrap the result", {
  identity_list <- function(a, b) list(a, b)

  for_all(
    a = anything(),
    b = anything(),
    property = function(a, b)
      maybe_map2(just(a), just(b), identity_list) %>%
        expect_identical(just(identity_list(a, b)))
  )
})
