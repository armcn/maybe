test_that("maybe_contains fails if '.m' isn't a maybe value", {
  for_all(
    a = anything(),
    property = \(a) maybe_contains(a, a) |> expect_error()
  )
})

test_that("maybe_contains returns FALSE if the maybe value is a nothing", {
  for_all(
    a = anything(),
    property = \(a) nothing() |> maybe_contains(a) |> expect_false()
  )
})

test_that("maybe_contains returns TRUE if the contents are equal to 'value'", {
  for_all(
    a = anything(),
    property = \(a) just(a) |> maybe_contains(a) |> expect_true()
  )
})
