test_that("not_infinite returns FALSE if value is infinite", {
  not_infinite(Inf) |> expect_false()
  not_infinite(-Inf) |> expect_false()
})

test_that("not_infinite returns TRUE if value is not infinite", {
  for_all(
    a = any_vector(),
    property = \(a) not_infinite(a) |> expect_true()
  )
})
