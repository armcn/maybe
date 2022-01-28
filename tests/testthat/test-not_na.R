test_that("not_na returns FALSE if value is NA", {
  not_na(NA) |> expect_false()
  not_na(NA_integer_) |> expect_false()
  not_na(NA_real_) |> expect_false()
  not_na(NA_character_) |> expect_false()
  not_na(NA_complex_) |> expect_false()
})

test_that("not_na returns TRUE if value is not NA", {
  for_all(
    a = anything(),
    property = \(a) (isTRUE(is.na(a)) || not_na(a)) |> expect_true()
  )
})
