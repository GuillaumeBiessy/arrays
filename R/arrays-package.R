#' arrays: Convenience functions for the manipulation of arrays
#'
#' The arrays package contains 7 functions for the manipulation of arrays and
#' conversion between data.frame (tibble) and arrays.
#'
#' @import purrr
#'
#' @docType package
#' @name arrays-package
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines and data.table
if (getRversion() >= "4.0")  {
  gv <- c("data.table", ".")
  utils::globalVariables(gv)
}

# library(devtools)

# # Packages
# use_package("purrr")
# use_package("rlang")
# use_package("reshape2")
# use_package("data.table")
# use_package("tibble")
# use_package("dplyr")

# # Data----

# Tests----
# use_test("test")

# # Patchnotes----
# use_news_md()
