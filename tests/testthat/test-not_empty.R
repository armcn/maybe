test_that("not_empty returns TRUE for non-empty vectors", {
  for_all(
    a = any_vector(len = c(1L, 10L)),
    property = \(a) not_empty(a) |> expect_true()
  )
})

test_that("not_empty returns TRUE for non-empty dataframes", {
  for_all(
    a = any_tibble(rows = c(1L, 10L)),
    property = \(a) not_empty(a) |> expect_true(),
    tests = 10L
  )
})

test_that("not_empty returns FALSE for empty vectors", {
  not_empty(vector()) |> expect_false()
})

test_that("not_empty returns FALSE for empty data frames", {
  not_empty(data.frame()) |> expect_false()
})
