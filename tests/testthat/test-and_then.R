test_that("and_then will fail with non-maybe values", {
  for_all(
    a = anything(),
    property = \(a)
      and_then(a, identity) |>
        expect_error()
  )
})

test_that("and_then will fail with a function which doesn't return a maybe value", {
  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        and_then(identity) |>
        expect_error()
  )
})

test_that("and_then will not modify a maybe value with a safe identity function", {
  safe_identity <- \(a) just(identity(a))

  nothing() |>
    and_then(safe_identity) |>
    expect_identical(nothing())

  for_all(
    a = anything(),
    property = \(a)
      just(a) |>
        and_then(safe_identity) |>
        expect_identical(just(a))
  )
})
