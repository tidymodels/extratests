# copied from https://github.com/tidymodels/tune/blob/main/tests/testthat/helper-tune-package.R
catalog_lines <- function(lines) {
  lines[grepl("^>", lines)]
}

# Make a new binding to prevent infinite recursion when the original is mocked.
initialize_catalog_ <- tune:::initialize_catalog

# Sets a new exit handler on `initialize_catalog()` that stores the summary
# of issues before it's cleared along with the progress bar. Together with
# the above, we can test the full catalog output.
redefer_initialize_catalog <- function(test_env) {
  local({
    function(control, env = rlang::caller_env()) {
      initialize_catalog_(control, env)

      withr::defer(
        assign(
          "catalog_summary_test",
          tune:::tune_env$progress_env$catalog_summary,
          test_env
        ),
        envir = env,
        priority = "first"
      )

      NULL
    }
  })
}

test_that("interactive logger works (finetune integration, error)", {
  skip_if(tune:::allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  local_mocked_bindings(
    is_testing = function() {FALSE},
    initialize_catalog = redefer_initialize_catalog(rlang::current_env()),
    .package = "tune"
  )
  library(finetune)

  raise_error <- function(x) {stop("AHHhH")}
  raise_warning <- function(x) {warning("ope! yikes.")}

  set.seed(1)
  expect_snapshot(
    {res_anova <-
      tune_race_anova(
        parsnip::nearest_neighbor("regression", "kknn", neighbors = tune()),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        control = control_race(extract = function(x) {raise_warning(); raise_error()})
      )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)

  set.seed(1)
  expect_snapshot(
    {res_sa <-
      tune_sim_anneal(
        parsnip::nearest_neighbor("regression", "kknn", neighbors = tune()),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        initial = res_anova,
        iter = 15,
        control = control_sim_anneal(verbose_iter = FALSE,
                                     extract = function(x) {raise_warning(); raise_error()})
      )},
    transform = catalog_lines
  )

  # `catalog_summary_test` written to this env via `redefer_initialize_catalog()`
  expect_snapshot(catalog_summary_test)
})
