test_that("not_empty returns TRUE for non-empty vectors", {
  for_all(
    a = any_vector(len = c(1L, 10L)),
    property = function(a) not_empty(a) %>% expect_true()
  )
})

test_that("not_empty returns TRUE for non-empty tibbles", {
  for_all(
    a = any_tibble(rows = c(1L, 10L)),
    property = function(a) not_empty(a) %>% expect_true()
  )
})

test_that("not_empty returns FALSE for empty vectors", {
  for_all(
    a = any_vector(len = 0L),
    property = function(a) not_empty(a) %>% expect_false()
  )
})

test_that("not_empty returns FALSE for empty tibbles", {
  for_all(
    a = any_tibble(rows = 0L),
    property = function(a) not_empty(a) %>% expect_false()
  )
})
