library(modeldata)
library(parsnip)

# Test tracker variables
.env_tests <- new.env()
.env_tests$spark_connection <- NULL

## -----------------------------------------------------------------------------

data("wa_churn")
data("lending_club")
data("hpc_data")

# ------------------------------------------------------------------------------

ctrl <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion("3.6.0", as.character(getRversion())) > 0

## --------------------------- Spark -------------------------------------------

skip_if_no_spark <- function() {
  skip <- NULL
  test_spark <- as.logical(Sys.getenv("TEST_SPARK", unset = TRUE))
  if (!isTRUE(test_spark)) {
    skip <- TRUE
  }
  if (is.null(skip)) {
    if (spark_not_installed()) {
      skip <- TRUE
    } else {
      if (is.null(spark_test_connection())) {
        skip <- TRUE
      }
    }
  }
  if (skip %||% FALSE) {
    skip("Skipping Spark related tests")
  } else {
    return(invisible())
  }
}

spark_not_installed <- function() {
  need_install <- purrr:::quietly(sparklyr::spark_install_find)()
  if (inherits(need_install, "try-error")) {
    need_install <- TRUE
  } else {
    need_install <- !isTRUE(need_install$result$installed)
  }
  need_install
}

spark_test_connection <- function() {
  suppressPackageStartupMessages(library(sparklyr))
  if (is.null(.env_tests$spark_connection)) {
    version <- Sys.getenv("SPARK_VERSION", unset = "3.5.7")
    sc <- try(sparklyr::spark_connect("local", version = version), silent = TRUE)
    if (inherits(sc, "try-error")) {
      return(NULL)
    }
    .env_tests$spark_connection <- sc
  }
  .env_tests$spark_connection
}

# ------------------------------------------------------------------------------

expect_ptype <- function(x, ptype) {
  expect_equal(x[0, names(ptype)], ptype)
}
