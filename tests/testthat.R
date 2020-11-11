library(testthat)
library(extratests)

## -----------------------------------------------------------------------------

spark_not_installed <- function() {

  need_install <- try(sparklyr::spark_install_find(), silent = TRUE)

  if(inherits(need_install, "try-error")) {
    need_install <- TRUE
  } else {
    need_install <- !isTRUE(need_install$installed)
  }
  need_install
}

## -----------------------------------------------------------------------------

test_check("extratests", reporter = "summary")
