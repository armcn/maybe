test_that("map_maybe will fail with a non-maybe value", {
  for_all(
    a = any_vector(),
    property = \(a) perhaps(map_maybe, default = "failure")(a, identity) |>
      expect_equal("failure")
  )
})

test_that("map_maybe will not change the input with identity function", {
  nothing() |> map_maybe(identity) |> is_nothing() |> expect_true()

  for_all(
    a = any_vector(),
    property = \(a) just(a) |> map_maybe(identity) |> expect_equal(just(a))
  )
})

test_that("map_maybe will return a nested maybe with a safe function", {
  safe_sqrt <- maybe(sqrt, ensure = not_nan)

  res_just <- just(1) |> map_maybe(safe_sqrt)
  res_just |> is_just() |> expect_true()
  res_just |> flatten_maybe() |> is_just() |> expect_true()

  res_nothing <- nothing() |> map_maybe(safe_sqrt)
  res_nothing |> is_nothing() |> expect_true()
  res_nothing |> flatten_maybe() |> is_nothing() |> expect_true()
})
