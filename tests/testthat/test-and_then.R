test_that("and_then will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = \(a) and_then(a, identity) |> expect_error()
  )
})

test_that("and_then will not modify a maybe value with the identity function", {
  nothing() |>
    and_then(identity) |>
    is_nothing() |>
    expect_true()

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        and_then(identity) |>
        expect_identical(just(a))
  )
})

test_that("and_then will behave the same as map_maybe with a regular function", {
  nothing() |>
    and_then(identity) |>
    expect_identical(map_maybe(nothing(), identity))

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        and_then(identity) |>
        expect_identical(map_maybe(just(a), identity))
  )
})

test_that("and_then will return a non-nested maybe with a maybe returning function", {
  nothing() |>
    and_then(identity) |>
    expect_identical(nothing())

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        and_then(identity) |>
        expect_identical(flatten_maybe(just(a)))
  )
})
