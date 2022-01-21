test_that("not_undefined returns FALSE if value is undefined", {
  not_undefined(NULL) |> expect_false()
  not_undefined(NA) |> expect_false()
  not_undefined(NA_integer_) |> expect_false()
  not_undefined(NA_real_) |> expect_false()
  not_undefined(NA_character_) |> expect_false()
  not_undefined(NA_complex_) |> expect_false()
  not_undefined(NaN) |> expect_false()
  not_undefined(-Inf) |> expect_false()
  not_undefined(Inf) |> expect_false()
})

test_that("not_undefined returns TRUE if value is not undefined", {
  for_all(
    a = any_vector(),
    property = \(a) not_undefined(a) |> expect_true()
  )
})
