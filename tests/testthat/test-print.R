test_that("print method prints correctly", {
  print(just(1)) |> expect_output("Just")
  print(just(1)) |> expect_output("1")

  print(nothing()) |> expect_output("Nothing")
})
