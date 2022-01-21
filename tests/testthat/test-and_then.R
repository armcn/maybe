test_that("and_then will fail with a non-maybe value", {
  for_all(
    a = any_vector(),
    property = \(a) perhaps(and_then, default = "failure")(a, identity) |>
      expect_equal("failure")
  )
})

test_that("and_then will not change the input with identity function", {
  nothing() |> and_then(identity) |> is_nothing() |> expect_true()

  for_all(
    a = any_vector(),
    property = \(a) just(a) |> and_then(identity) |> expect_equal(just(a))
  )
})

test_that("and_then will return a non-nested maybe value with a safe function", {
  safe_sqrt <- maybe(sqrt, ensure = not_nan)

  res_just <- just(1) |> and_then(safe_sqrt)
  res_just |> flatten_maybe() |> expect_equal(res_just)

  res_nothing <- nothing() |> and_then(safe_sqrt)
  res_nothing |> flatten_maybe() |> expect_equal(res_nothing)
})
