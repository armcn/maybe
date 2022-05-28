test_that("maybe_case will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = function(a)
      maybe_case(a, identity, default = 0) %>%
        expect_error()
  )
})

test_that("maybe_case unwraps and calls a function on a just value", {
  for_all(
    a = anything(),
    property = function(a)
      just(a) %>%
        maybe_case(list, default = 0) %>%
        expect_identical(list(a))
  )
})

test_that("maybe_case returns the default with a nothing value", {
  for_all(
    a = anything(),
    property = function(a)
      nothing() %>%
        maybe_case(list, default = a) %>%
        expect_identical(a)
  )
})
