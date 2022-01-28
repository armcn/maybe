test_that("maybe modifies a function to return a maybe value", {
  for_all(
    a = anything(),
    property = \(a) maybe(identity)(a) |> is_maybe() |> expect_true()
  )
})

test_that("maybe modifies a function to return a just value", {
  for_all(
    a = anything(),
    property = \(a) maybe(identity)(a) |> is_just() |> expect_true()
  )
})

test_that("maybe modifies a function to return nothing when it has warnings", {
  for_all(
    a = anything(),
    property = \(a)
      maybe(\(b) warning(""))(a) |>
        is_nothing() |>
        expect_true()
  )
})

test_that("maybe modifies a function to return nothing when it has errors", {
  for_all(
    a = anything(),
    property = \(a)
      maybe(\(b) stop(""))(a) |>
        is_nothing() |>
        expect_true()
  )
})

test_that("allow_warning allows functions with warnings to return just", {
  for_all(
    a = anything(),
    property = \(a)
      maybe(\(a) { warning(""); a }, allow_warning = TRUE)(a) |>
        is_just() |>
        expect_true()
  )
})

test_that("maybe will return a just value if the predicate returns TRUE", {
  for_all(
    a = any_atomic(),
    property = \(a)
      maybe(identity, ensure = is.atomic)(a) |>
        is_just() |>
        expect_true()
  )
})

test_that("maybe will return nothing if the predicate returns FALSE", {
  for_all(
    a = any_undefined(),
    property = \(a)
      maybe(identity, ensure = not_undefined)(a) |>
        is_nothing() |>
        expect_true()
  )
})
