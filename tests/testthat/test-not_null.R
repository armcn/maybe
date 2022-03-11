test_that("not_null returns FALSE if value is NULL", {
  not_null(NULL) %>% expect_false()
})

test_that("not_null returns TRUE if value is not NULL", {
  for_all(
    a = anything(),
    property = function(a) (is.null(a) || not_null(a)) %>% expect_true()
  )
})
