test_that("maybe_map will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = \(a) maybe_map(a, identity) |> expect_error()
  )
})

test_that("maybe_map will not modify a maybe value with the identity function", {
  nothing() |>
    maybe_map(identity) |>
    is_nothing() |>
    expect_true()

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        maybe_map(identity) |>
        expect_identical(just(a))
  )
})

test_that("maybe_map will return a nested just with a maybe returning function", {
  safe_identity <- function(a) just(identity(a))

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        maybe_map(safe_identity) |>
        expect_identical(just(just(a)))
  )
})
