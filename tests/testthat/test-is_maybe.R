test_that("is_maybe returns TRUE for maybe values", {
  is_maybe(just(1)) |> expect_true()
  is_maybe(nothing()) |> expect_true()
})

test_that("is_maybe returns FALSE for non-maybe values", {
  for_all(
    a = any_vector(),
    property = \(a) is_maybe(a) |> expect_false()
  )
})
