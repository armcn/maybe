test_that("flatten_maybe will fail with a non-maybe value", {
  for_all(
    a = anything(),
    property = \(a) flatten_maybe(a) |> expect_error()
  )
})

test_that("flatten_maybe doesn't modify non-nested maybes", {
  nothing() |> flatten_maybe() |> expect_identical(nothing())

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        flatten_maybe() |>
        expect_identical(just(a))
  )
})

test_that("flatten_maybe removes a layer from a nested maybe", {
  just(nothing()) |> flatten_maybe() |> expect_identical(nothing())

  for_all(
    a = anything(),
    property = \(a)
      just(just(a)) |>
        flatten_maybe() |>
        expect_identical(just(a))
  )
})
