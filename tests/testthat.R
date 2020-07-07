library(testthat)
library(extratests)

## -----------------------------------------------------------------------------

library(sparklyr)

if (.Platform$OS.type == "windows") {
  sparklyr::spark_install(verbose = TRUE, version = "2.4", hadoop_version = "2.7")
} else {
  sparklyr::spark_install(verbose = TRUE, version = "2.4")
}

sc <- try(sparklyr::spark_connect(master = "local"), silent = TRUE)

if(inherits(sc, "try-error")) {
  print(sc)
}

## -----------------------------------------------------------------------------

test_check("extratests", reporter = "summary")
