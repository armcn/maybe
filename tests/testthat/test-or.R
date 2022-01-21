test_that("or doesn't change anything with a single predicate", {
  or(is.integer)(1L) |> expect_equal(is.integer(1L))
  or(is.character)(1L) |> expect_equal(is.character(1L))
})

test_that("or returns a function that returns TRUE if any are TRUE", {
  or(\(a) FALSE, \(a) TRUE)(1) |> expect_true()
  or(\(a) TRUE, \(a) FALSE)(1) |> expect_true()
  or(\(a) TRUE, \(a) TRUE)(1) |> expect_true()
})

test_that("or returns a function that returns FALSE if all are FALSE", {
  or(\(a) FALSE)(1) |> expect_false()
  or(\(a) FALSE, \(a) FALSE)(1) |> expect_false()
})
