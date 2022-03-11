test_that("maybe_equal fails if either argument isn't a maybe value", {
  maybe_equal(nothing(), 1) %>% expect_error()
  maybe_equal(1, nothing()) %>% expect_error()

  for_all(
    a = anything(),
    property = function(a) {
      maybe_equal(a, a) %>% expect_error()
      maybe_equal(just(a), a) %>% expect_error()
      maybe_equal(a, just(a)) %>% expect_error()
    }
  )
})

test_that("maybe_equal returns TRUE if arguments are equal", {
  maybe_equal(nothing(), nothing()) %>% expect_true()

  for_all(
    a = anything(),
    property = function(a) maybe_equal(just(a), just(a)) %>% expect_true()
  )
})

test_that("maybe_equal returns FALSE if arguments are not equal", {
  for_all(
    a = anything(),
    property = function(a) {
      maybe_equal(just(a), nothing()) %>% expect_false()
      maybe_equal(nothing(), just(a)) %>% expect_false()
      maybe_equal(just(a), just(list(a))) %>% expect_false()
    }
  )
})
