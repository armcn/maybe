library(quickcheck)

is_dev_version <- function() {
  version_length <-
    utils::packageDescription("maybe")$Version %>%
      strsplit("\\.") %>%
      {function(a) length(a[[1]])}()

  version_length > 3L
}

setup_tests <- function() {
  if (is_dev_version())
    options(quickcheck.tests = 100L)

  else
    options(quickcheck.tests = 5L)
}

setup_tests()
