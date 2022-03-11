test_that("just replaces class with 'maybe'", {
  for_all(
    a = anything(),
    property = function(a) just(a) %>% class() %>% expect_identical("maybe")
  )
})

test_that("just returns maybe values", {
  for_all(
    a = anything(),
    property = function(a) just(a) %>% is_maybe() %>% expect_true()
  )
})

test_that("just returns just values", {
  for_all(
    a = anything(),
    property = function(a) just(a) %>% is_just() %>% expect_true()
  )
})
