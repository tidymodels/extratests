library(testthat)
library(extratests)

## -----------------------------------------------------------------------------

spark_installed <- function() {
  sparklyr::spark_install_find()$installed
}

## -----------------------------------------------------------------------------

test_check("extratests", reporter = "summary")
