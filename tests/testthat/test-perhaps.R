test_that("perhaps doesn't change a function if it doesn't warning or error", {
  perhaps(identity, default = 0)(1) |> expect_equal(identity(1))
})

test_that("perhaps modifies a function to the default when warnings", {
  perhaps(\(a) warning("a warning"), default = 0)(1) |> expect_equal(0)
})

test_that("perhaps modifies a function to return the default when errors", {
  perhaps(\(a) stop("an error"), default = 0)(1) |> expect_equal(0)
})

test_that("allow_warning allows functions with warnings to return expected", {
  suppressWarnings(
    perhaps(
      \(a) { warning("a warning"); a },
      default = 0,
      allow_warning = TRUE
    )(1) |>
      expect_equal(1)
  )
})

test_that("will return expected value if a predicate returns TRUE", {
  perhaps(\(a) a, default = 0, ensure = is.character)("hello") |>
    expect_equal("hello")
})

test_that("will return the default if a predicate returns FALSE", {
  perhaps(\(a) a, default = 0, ensure = is.character)(1) |>
    expect_equal(0)
})

