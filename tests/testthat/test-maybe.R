test_that("maybe modifies a function to return a maybe value", {
  maybe(identity)(1) |> is_maybe() |> expect_true()
})

test_that("maybe modifies a function to return a just value when no errors", {
  maybe(identity)(1) |> is_just() |> expect_true()
})

test_that("maybe modifies a function to return nothing when warnings", {
  maybe(\(a) warning("a warning"))(1) |> is_nothing() |> expect_true()
})

test_that("maybe modifies a function to return nothing when errors", {
  maybe(\(a) stop("an error"))(1) |> is_nothing() |> expect_true()
})

test_that("allow_warning allows functions with warnings to return just", {
  suppressWarnings(
    maybe(\(a) { warning("a warning"); a }, allow_warning = TRUE)(1) |>
      is_just() |>
      expect_true()
  )
})

test_that("will return just value if a predicate returns TRUE", {
  maybe(\(a) a, ensure = is.character)("hello") |> is_just() |> expect_true()
})

test_that("will return nothing if a predicate returns FALSE", {
  maybe(\(a) a, ensure = is.character)(1) |> is_nothing() |> expect_true()
})
