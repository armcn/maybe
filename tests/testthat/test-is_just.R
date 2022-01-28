test_that("is_just returns TRUE for just values", {
  for_all(
    a = anything(),
    property = \(a) just(a) |> is_just() |> expect_true()
  )
})

test_that("is_just returns FALSE for non-just values", {
  nothing() |> is_just() |> expect_false()

  for_all(
    a = anything(),
    property = \(a) is_just(a) |> expect_false()
  )
})
