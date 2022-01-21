test_that("flatten_maybe will fail with a non-maybe value", {
  for_all(
    a = any_vector(),
    property = \(a) perhaps(flatten_maybe, default = "failure")(a) |>
      expect_equal("failure")
  )
})

test_that("flatten_maybe doesn't change non-nested maybes", {
  nothing() |> flatten_maybe() |> expect_identical(nothing())

  for_all(
    a = any_vector(),
    property = \(a) just(a) |> flatten_maybe() |> expect_identical(just(a))
  )
})

test_that("flatten_maybe removes a layer from a nested maybe", {
  just(nothing()) |> flatten_maybe() |> expect_identical(nothing())

  for_all(
    a = any_vector(),
    property = \(a)
      just(just(1)) |>
      flatten_maybe() |>
      expect_identical(just(1))
  )
})
