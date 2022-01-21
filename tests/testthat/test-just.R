test_that("any value wrapped in just has class replaced by 'maybe'", {
  for_all(
    a = any_vector(),
    property = \(a) just(a) |> class() |> expect_equal("maybe")
  )
})

test_that("any value wrapped in just is a maybe value", {
  for_all(
    a = any_vector(),
    property = \(a) just(a) |> is_maybe() |> expect_true()
  )
})

test_that("any value wrapped in just is a just value", {
  for_all(
    a = any_vector(),
    property = \(a) just(a) |> is_just() |> expect_true()
  )
})
