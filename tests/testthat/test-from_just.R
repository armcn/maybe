test_that("from_maybe will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = function(a) from_maybe(a) %>% expect_error()
  )
})

test_that("from_maybe will fail with nothing", {
   nothing() %>% from_maybe() %>% expect_error()
})

test_that("from_maybe unwraps a just value", {
  for_all(
    a = anything(),
    property = function(a)
      just(a) %>%
        from_maybe() %>%
        expect_identical(a)
  )
})
