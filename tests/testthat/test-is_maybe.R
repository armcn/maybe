test_that("is_maybe returns TRUE for maybe values", {
  nothing() %>% is_maybe() %>% expect_true()

  for_all(
    a = anything(),
    property = function(a) just(a) %>% is_maybe() %>% expect_true()
  )
})

test_that("is_maybe returns FALSE for non-maybe values", {
  for_all(
    a = anything(),
    property = function(a) is_maybe(a) %>% expect_false()
  )
})
