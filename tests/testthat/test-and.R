test_that("and doesn't modify a single predicate", {
  for_all(
    a = anything(),
    property = function(a) and(is.null)(a) %>% expect_identical(is.null(a))
  )
})

test_that("and returns a function that returns FALSE if any are FALSE", {
  and(function(a) FALSE, function(a) TRUE)(NULL) %>% expect_false()
  and(function(a) TRUE, function(a) FALSE)(NULL) %>% expect_false()
  and(function(a) FALSE, function(a) FALSE)(NULL) %>% expect_false()
})

test_that("and returns a function that returns TRUE if all are TRUE", {
  and(function(a) TRUE)(NULL) %>% expect_true()
  and(function(a) TRUE, function(a) TRUE)(NULL) %>% expect_true()
})
