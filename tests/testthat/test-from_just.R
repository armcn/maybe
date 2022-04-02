test_that("from_just will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = function(a) from_just(a) %>% expect_error()
  )
})

test_that("from_just will fail with nothing", {
   nothing() %>% from_just() %>% expect_error()
})

test_that("from_just unwraps a just value", {
  for_all(
    a = anything(),
    property = function(a)
      just(a) %>%
        from_just() %>%
        expect_identical(a)
  )
})
