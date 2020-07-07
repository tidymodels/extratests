library(testthat)
library(extratests)

## -----------------------------------------------------------------------------

library(sparklyr)

sparklyr::spark_install(verbose = TRUE)

sc <- try(sparklyr::spark_connect(master = "local"), silent = TRUE)

if(inherits(sc, "try-error")) {
  print(sc)
}

## -----------------------------------------------------------------------------

test_check("extratests", reporter = "summary")
