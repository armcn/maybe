test_that("filter_map will return an empty list if '.l' is an empty list", {
  return_nothing <- function(a) nothing()

  filter_map(list(), return_nothing) %>%
    expect_identical(list())

  filter_map(list(), just) %>%
    expect_identical(list())
})

test_that("filter_map will return an empty list if all values are nothing", {
  return_nothing <- function(a) nothing()

  list(1) %>%
    filter_map(return_nothing) %>%
    expect_identical(list())

  list(1, 2) %>%
    filter_map(return_nothing) %>%
    expect_identical(list())
})

test_that("filter_map will unwrap values if all values are justs", {
  for_all(
    a = any_list(),
    property = function(a) {
      filter_map(a, just) %>%
        expect_identical(a)
    }
  )
})

test_that("filter_map will drop nothings", {
  parse_numeric <- maybe(as.numeric)

  list("a") %>%
    filter_map(parse_numeric) %>%
    expect_identical(list())

  list(1, "a", "b") %>%
    filter_map(parse_numeric) %>%
    expect_identical(list(1))
})
