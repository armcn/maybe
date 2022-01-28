test_that("is_nothing returns TRUE for nothing values", {
  nothing() |> is_nothing() |> expect_true()
})

test_that("is_nothing returns FALSE for non-nothing values", {
  for_all(
    a = anything(),
    property = \(a) just(a) |> is_nothing() |> expect_false()
  )

  for_all(
    a = anything(),
    property = \(a) is_nothing(a) |> expect_false()
  )
})
