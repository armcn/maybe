test_that("not_infinite returns FALSE if value is infinite", {
  not_infinite(Inf) |> expect_false()
  not_infinite(-Inf) |> expect_false()
})

test_that("not_infinite returns TRUE if value is not infinite", {
  for_all(
    a = anything(),
    property = \(a)
      (identical(a, Inf) || identical(a, -Inf) || not_infinite(a)) |>
        expect_true()
  )
})
