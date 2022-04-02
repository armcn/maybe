test_that("filter_justs will fail if '.l' isn't a list of maybe values", {
  for_all(
    a = anything(),
    property = function(a)
      filter_justs(a) %>% expect_error()
  )
})

test_that("filter_justs will fail if any values aren't maybes", {
  list(1) %>%
    filter_justs() %>%
    expect_error()

  list(1, just(2)) %>%
    filter_justs() %>%
    expect_error()
})

test_that("filter_justs will return an empty list if all values are nothing", {
  list(nothing()) %>%
    filter_justs() %>%
    expect_identical(list())

  list(nothing(), nothing()) %>%
    filter_justs() %>%
    expect_identical(list())
})

test_that("filter_justs will unwrap values if all values are justs", {
  for_all(
    a = any_list(),
    property = function(a) {
      lapply(a, just) %>%
        filter_justs() %>%
        expect_identical(a)
    }
  )
})

test_that("filter_justs will drop nothings", {
  list(nothing()) %>%
    filter_justs() %>%
    expect_identical(list())

  list(just(1), just("a"), nothing()) %>%
    filter_justs() %>%
    expect_identical(list(1, "a"))
})
