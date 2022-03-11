test_that("print method prints correctly", {
  print(nothing()) %>% expect_output("Nothing")

  print(just(1)) %>% expect_output("Just")
  print(just(1)) %>% expect_output("1")
})
