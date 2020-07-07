library(testthat)
library(extratests)

## -----------------------------------------------------------------------------

library(sparklyr)

if (.Platform$OS.type == "windows") {
  sparklyr::spark_install(verbose = TRUE, hadoop_version = "2.6.0")
} else {
  sparklyr::spark_install(verbose = TRUE)
}

sc <- try(sparklyr::spark_connect(master = "local"), silent = TRUE)

if(inherits(sc, "try-error")) {
  print(sc)
}

## -----------------------------------------------------------------------------

test_check("extratests", reporter = "summary")
