suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9014")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("compute_metrics works with survival models", {
  lung_surv <- lung %>%
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")

  metrics <- metric_set(
    concordance_survival,
    brier_survival_integrated,
    brier_survival
  )

  times <- c(2, 50, 100)

  set.seed(2193)
  tune_res <-
    proportional_hazards(penalty = tune(), engine = "glmnet") %>%
    tune_grid(
      surv ~ .,
      resamples = vfold_cv(lung_surv, 2),
      grid = tibble(penalty = c(0.001, 0.1)),
      control = control_grid(save_pred = TRUE),
      metrics = metrics,
      eval_time = times
    )

  # ------------------------------------------------------------------------------

  recomp <- compute_metrics(tune_res, metrics, summarize = TRUE)
  original <- collect_metrics(tune_res)
  expect_equal(recomp, original)

  # ------------------------------------------------------------------------------

  recomp_rs <- compute_metrics(tune_res, metrics, summarize = TRUE)
  original_rs <- collect_metrics(tune_res, summarize = TRUE)
  expect_equal(recomp_rs, original_rs)

  # ------------------------------------------------------------------------------

  stc_only <- compute_metrics(
    tune_res,
    metric_set(concordance_survival),
    summarize = TRUE
  )
  stc_original <- collect_metrics(tune_res) %>%
    filter(.metric == "concordance_survival")
  expect_equal(stc_only, stc_original %>% select(-.eval_time))
})
