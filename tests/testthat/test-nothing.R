test_that("nothing returns a maybe value", {
  nothing() %>% is_maybe() %>% expect_true()
})

test_that("nothing returns a nothing value", {
  nothing() %>% is_nothing() %>% expect_true()
})
