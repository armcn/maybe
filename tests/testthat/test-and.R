test_that("and doesn't modify a single predicate", {
  for_all(
    a = anything(),
    property = \(a) and(is.null)(a) |> expect_identical(is.null(a))
  )
})

test_that("and returns a function that returns FALSE if any are FALSE", {
  and(\(a) FALSE, \(a) TRUE)(NULL) |> expect_false()
  and(\(a) TRUE, \(a) FALSE)(NULL) |> expect_false()
  and(\(a) FALSE, \(a) FALSE)(NULL) |> expect_false()
})

test_that("and returns a function that returns TRUE if all are TRUE", {
  and(\(a) TRUE)(NULL) |> expect_true()
  and(\(a) TRUE, \(a) TRUE)(NULL) |> expect_true()
})
