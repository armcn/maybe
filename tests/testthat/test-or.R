test_that("or doesn't modify a single predicate", {
  for_all(
    a = anything(),
    property = function(a) or(is.null)(a) %>% expect_identical(is.null(a))
  )
})

test_that("or returns a function that returns TRUE if any are TRUE", {
  or(function(a) FALSE, function(a) TRUE)(NULL) %>% expect_true()
  or(function(a) TRUE, function(a) FALSE)(NULL) %>% expect_true()
  or(function(a) TRUE, function(a) TRUE)(NULL) %>% expect_true()
})

test_that("or returns a function that returns FALSE if all are FALSE", {
  or(function(a) FALSE)(NULL) %>% expect_false()
  or(function(a) FALSE, function(a) FALSE)(NULL) %>% expect_false()
})
