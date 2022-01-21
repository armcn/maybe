test_that("is_just returns TRUE for just values", {
  for_all(
    a = any_vector(),
    property = \(a) just(a) |> is_just() |> expect_true()
  )
})

test_that("is_just returns FALSE for non-just values", {
  is_just(nothing()) |> expect_false()

  for_all(
    a = any_vector(),
    property = \(a) is_just(a) |> expect_false()
  )
})
