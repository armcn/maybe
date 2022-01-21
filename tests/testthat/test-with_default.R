test_that("with_default will fail with a non-maybe value", {
  for_all(
    a = any_vector(),
    property = \(a)
      perhaps(with_default, default = "failure")(a, default = 0) |>
      expect_equal("failure")
  )
})

test_that("with_default will unwrap a just value", {
  for_all(
    a = any_vector(),
    property = \(a) just(a) |> with_default(default = 0) |> expect_equal(a)
  )
})

test_that("with_default will return default with a nothing value", {
  for_all(
    a = any_vector(),
    property = \(a) nothing() |> with_default(default = a) |> expect_equal(a)
  )
})
