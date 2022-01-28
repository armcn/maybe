library(quickcheck)

setup_tests <- function() {
  if (is_dev_version())
    options(quickcheck.tests = 100L)

  else
    options(quickcheck.tests = 5L)
}

is_dev_version <- function() {
  version_length <-
    utils::packageDescription("quickcheck")$Version |>
      strsplit("\\.") |>
      {\(a) length(a[[1]])}()

  version_length > 3L
}

setup_tests()
