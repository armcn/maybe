test_that("with_default will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = function(a) with_default(a, default = 0) %>% expect_error()
  )
})

test_that("with_default unwraps a just value", {
  for_all(
    a = anything(),
    property = function(a)
      just(a) %>%
        with_default(default = 0) %>%
        expect_identical(a)
  )
})

test_that("with_default returns the default with a nothing value", {
  for_all(
    a = anything(),
    property = function(a)
      nothing() %>%
        with_default(default = a) %>%
        expect_identical(a)
  )
})
