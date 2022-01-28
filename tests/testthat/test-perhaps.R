test_that("perhaps doesn't modify a function if it doesn't error or warn", {
  for_all(
    a = anything(),
    property = \(a)
      perhaps(identity, default = 0)(a) |>
        expect_identical(a)
  )
})

test_that("perhaps modifies a function to return a default when it has warnings", {
  for_all(
    a = anything(),
    property = \(a)
      perhaps(\(b) warning(""), default = a)(NULL) |>
        expect_identical(a)
  )
})

test_that("perhaps modifies a function to return a default when it has errors", {
  for_all(
    a = anything(),
    property = \(a)
      perhaps(\(b) stop(""), default = a)(NULL) |>
        expect_identical(a)
  )
})

test_that("allow_warning allows functions with warnings to return expected", {
  for_all(
    a = anything(),
    property = \(a)
      perhaps(\(b) { warning(""); b }, allow_warning = TRUE)(a) |>
        expect_equal(a)
  )
})

test_that("perhaps will return the expected value if the predicate returns TRUE", {
  for_all(
    a = any_atomic(),
    property = \(a)
      perhaps(identity, ensure = is.atomic, default = 0)(a) |>
        expect_identical(a)
  )
})

test_that("perhaps will return the default if the predicate returns FALSE", {
  for_all(
    a = any_undefined(),
    property = \(a)
      perhaps(identity, ensure = not_undefined, default = a)(NULL) |>
        expect_identical(a)
  )
})
