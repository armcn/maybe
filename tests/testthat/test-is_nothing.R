test_that("is_nothing returns TRUE for nothing values", {
  is_nothing(nothing()) |> expect_true()
})

test_that("is_nothing returns FALSE for non-nothing values", {
  is_nothing(just(1)) |> expect_false()

  for_all(
    a = any_vector(),
    property = \(a) is_nothing(a) |> expect_false()
  )
})
