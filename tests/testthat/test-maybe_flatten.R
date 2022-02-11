test_that("maybe_flatten will fail with a non-maybe value", {
  for_all(
    a = anything(),
    property = \(a) maybe_flatten(a) |> expect_error()
  )
})

test_that("maybe_flatten doesn't modify non-nested maybes", {
  nothing() |> maybe_flatten() |> expect_identical(nothing())

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        maybe_flatten() |>
        expect_identical(just(a))
  )
})

test_that("maybe_flatten removes a layer from a nested maybe", {
  just(nothing()) |> maybe_flatten() |> expect_identical(nothing())

  for_all(
    a = anything(),
    property = \(a)
      just(just(a)) |>
        maybe_flatten() |>
        expect_identical(just(a))
  )
})
