library(modeldata)
library(parsnip)

## -----------------------------------------------------------------------------

data("wa_churn")
data("lending_club")
data("hpc_data")

# ------------------------------------------------------------------------------

ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0

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
