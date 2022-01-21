test_that("and doesn't change anything with a single predicate", {
  and(is.integer)(1L) |> expect_equal(is.integer(1L))
  and(is.character)(1L) |> expect_equal(is.character(1L))
})

test_that("and returns a function that returns FALSE if any are FALSE", {
  and(\(a) FALSE, \(a) TRUE)(1) |> expect_false()
  and(\(a) TRUE, \(a) FALSE)(1) |> expect_false()
  and(\(a) FALSE, \(a) FALSE)(1) |> expect_false()
})

test_that("and returns a function that returns TRUE if all are TRUE", {
  and(\(a) TRUE)(1) |> expect_true()
  and(\(a) TRUE, \(a) TRUE)(1) |> expect_true()
})
