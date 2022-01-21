test_that("not_null returns FALSE if value is NULL", {
  not_null(NULL) |> expect_false()
})

test_that("not_null returns TRUE if value is not NULL", {
  for_all(
    a = any_vector(),
    property = \(a) not_null(a) |> expect_true()
  )
})
