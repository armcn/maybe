test_that("filter_map will return an empty list if all values are nothing", {
  list(nothing()) %>%
    filter_map(identity) %>%
    expect_identical(list())

  list(nothing(), nothing()) %>%
    filter_map(identity) %>%
    expect_identical(list())
})

test_that("filter_map will unwrap values if all values are justs", {
  for_all(
    a = any_list(),
    property = function(a) {
      lapply(a, just) %>%
        filter_map(identity) %>%
        expect_identical(a)
    }
  )
})

test_that("filter_map will drop nothings", {
  list(nothing()) %>%
    filter_map(identity) %>%
    expect_identical(list())

  list(just(1), just("a"), nothing()) %>%
    filter_map(identity) %>%
    expect_identical(list(1, "a"))
})
