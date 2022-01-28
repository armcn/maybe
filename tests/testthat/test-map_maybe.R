test_that("map_maybe will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = \(a) map_maybe(a, identity) |> expect_error()
  )
})

test_that("map_maybe will not modify a maybe value with the identity function", {
  nothing() |>
    map_maybe(identity) |>
    is_nothing() |>
    expect_true()

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        map_maybe(identity) |>
        expect_identical(just(a))
  )
})

test_that("map_maybe will return a nested just with a maybe returning function", {
  safe_identity <- function(a) just(identity(a))

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        map_maybe(safe_identity) |>
        expect_identical(just(just(a)))
  )
})
