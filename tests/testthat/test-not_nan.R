test_that("not_nan returns FALSE if value is NaN", {
  not_nan(NaN) |> expect_false()
})

test_that("not_nan returns TRUE if value is not NaN", {
  for_all(
    a = anything(),
    property = \(a)
      (isTRUE(identical(a, NaN)) || not_nan(a)) |>
        expect_true()
  )
})
