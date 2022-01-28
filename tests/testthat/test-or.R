test_that("or doesn't modify a single predicate", {
  for_all(
    a = anything(),
    property = \(a) or(is.null)(a) |> expect_identical(is.null(a))
  )
})

test_that("or returns a function that returns TRUE if any are TRUE", {
  or(\(a) FALSE, \(a) TRUE)(NULL) |> expect_true()
  or(\(a) TRUE, \(a) FALSE)(NULL) |> expect_true()
  or(\(a) TRUE, \(a) TRUE)(NULL) |> expect_true()
})

test_that("or returns a function that returns FALSE if all are FALSE", {
  or(\(a) FALSE)(NULL) |> expect_false()
  or(\(a) FALSE, \(a) FALSE)(NULL) |> expect_false()
})
