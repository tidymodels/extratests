test_that("evaluation time argument to tune objects", {
  skip_if_not_installed("yardstick", minimum_version = "1.1.0.9000")

  suppressPackageStartupMessages(library(tune))
  suppressPackageStartupMessages(library(censored))
  suppressPackageStartupMessages(library(yardstick))
  suppressPackageStartupMessages(library(rsample))

  spec <- survival_reg()
  set.seed(1)
  rs <- vfold_cv(stanford2, strata = status)
  .time <- seq(1, 1000, length = 5)
  mtr <- metric_set(brier_survival)
  reg_mtr <- metric_set(rmse)

  expect_snapshot(error = TRUE,
                  spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr)
  )
  expect_snapshot(error = TRUE,
                  spec %>% tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = reg_mtr)
  )
  expect_snapshot(
    linear_reg() %>% tune_grid(age ~ ., resamples = rs, metrics = reg_mtr, eval_time = 1)
  )

  expect_snapshot(error = TRUE,
                  no_usable_times <-
                    spec %>%
                    tune_grid(Surv(time, status) ~ ., resamples = rs, metrics = mtr, eval_time = c(-1, Inf))
  )

  times <- 4:1
  expect_equal(get_metric_time(metric_set(brier_survival), times), 4)
  expect_equal(get_metric_time(metric_set(concordance_survival), times), NULL)
  expect_equal(get_metric_time(metric_set(brier_survival_integrated), times), NULL)
  expect_equal(
    get_metric_time(
      metric_set(brier_survival, brier_survival_integrated, concordance_survival),
      times
    ),
    4
  )

})


test_that("eval time inputs are checked for censored regression models", {
  skip_if_not_installed("censored")

  library(parsnip)
  library(workflows)
  library(yardstick)
  library(rsample)
  suppressPackageStartupMessages(library(censored))

  stanford2$event_time <- Surv(stanford2$time, stanford2$status)
  stanford2 <- stanford2[, c("event_time", "age")]

  wflow <- workflow(event_time ~ age, survival_reg())
  sr_spec <- survival_reg(dist = tune())
  wflow_tune <- workflow(event_time ~ age, sr_spec)

  set.seed(1)
  split <- initial_split(stanford2)
  rs <- vfold_cv(stanford2)

  # ------------------------------------------------------------------------------
  # setup metric sets

  met_stc <- metric_set(concordance_survival)
  met_dyn <- metric_set(brier_survival)
  met_int <- metric_set(brier_survival_integrated)
  met_stc_dyn <- metric_set(concordance_survival, brier_survival)
  met_stc_int <- metric_set(concordance_survival, brier_survival_integrated)
  met_dyn_stc <- metric_set(brier_survival, concordance_survival)
  met_dyn_int <- metric_set(brier_survival, brier_survival_integrated)
  met_int_stc <- metric_set(brier_survival_integrated, concordance_survival)
  met_int_dyn <- metric_set(brier_survival_integrated, brier_survival)

  # ------------------------------------------------------------------------------
  # check inputs when eval_time left out

  expect_snapshot(check_eval_time_arg(NULL, met_stc))
  expect_snapshot(check_eval_time_arg(NULL, met_dyn), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(NULL, met_stc_dyn), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_stc_int), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_dyn_stc), error = TRUE)

  expect_snapshot(check_eval_time_arg(NULL, met_dyn_int), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_int_stc), error = TRUE)
  expect_snapshot(check_eval_time_arg(NULL, met_int_dyn), error = TRUE)

  # ------------------------------------------------------------------------------
  # check inputs with single eval times

  expect_snapshot(check_eval_time_arg(2, met_stc))
  expect_snapshot(check_eval_time_arg(2, met_dyn))
  expect_snapshot(check_eval_time_arg(2, met_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(2, met_stc_dyn))
  expect_snapshot(check_eval_time_arg(2, met_stc_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(2, met_dyn_stc))
  expect_snapshot(check_eval_time_arg(2, met_dyn_int), error = TRUE)

  expect_snapshot(check_eval_time_arg(2, met_int_stc), error = TRUE)
  expect_snapshot(check_eval_time_arg(2, met_int_dyn), error = TRUE)

  # ------------------------------------------------------------------------------
  # check inputs with multiple eval times

  expect_snapshot(check_eval_time_arg(1:3, met_stc))
  expect_snapshot(check_eval_time_arg(1:3, met_dyn))
  expect_snapshot(check_eval_time_arg(1:3, met_int))

  expect_snapshot(check_eval_time_arg(1:3, met_stc_dyn))
  expect_snapshot(check_eval_time_arg(1:3, met_stc_int))
  expect_snapshot(check_eval_time_arg(1:3, met_dyn_stc))

  expect_snapshot(check_eval_time_arg(1:3, met_dyn_int))
  expect_snapshot(check_eval_time_arg(1:3, met_int_stc))
  expect_snapshot(check_eval_time_arg(1:3, met_int_dyn))

  # ------------------------------------------------------------------------------
  # resampling

  # no eval time
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int), error = TRUE)

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_stc_dyn), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_stc_int), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn_stc), error = TRUE)

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn_int), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_stc), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_dyn), error = TRUE)

  # one eval time
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 2))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 2))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 2))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 2))
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 2), error = TRUE)

  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 2), error = TRUE)
  expect_snapshot(fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 2), error = TRUE)

  # multiple eval times
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_int, eval_time = 1:3))

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc_dyn, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_stc_int, eval_time = 1:3))

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn_stc, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_dyn_int, eval_time = 1:3))

  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_int_stc, eval_time = 1:3))
  expect_snapshot(res <- fit_resamples(wflow, rs, metrics = met_int_dyn, eval_time = 1:3))

  # ------------------------------------------------------------------------------
  # grid tuning (tune bayes tests in extratests repo)

  # no eval time
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int), error = TRUE)

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_stc_dyn), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_stc_int), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn_stc), error = TRUE)

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn_int), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_stc), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_dyn), error = TRUE)

  # one eval time
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 2))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 2))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 2))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 2))
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 2), error = TRUE)

  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 2), error = TRUE)
  expect_snapshot(tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 2), error = TRUE)

  # multiple eval times
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_int, eval_time = 1:3))

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc_dyn, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_stc_int, eval_time = 1:3))

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn_stc, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_dyn_int, eval_time = 1:3))

  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_int_stc, eval_time = 1:3))
  expect_snapshot(res <- tune_grid(wflow_tune, rs, metrics = met_int_dyn, eval_time = 1:3))

  # ------------------------------------------------------------------------------
  # last fit

  # no eval time
  expect_silent(res <- last_fit(wflow, split, metrics = met_stc))
  expect_snapshot(last_fit(wflow, split, metrics = met_dyn), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int), error = TRUE)

  expect_snapshot(last_fit(wflow, split, metrics = met_stc_dyn), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_stc_int), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_dyn_stc), error = TRUE)

  expect_snapshot(last_fit(wflow, split, metrics = met_dyn_int), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int_stc), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int_dyn), error = TRUE)

  # one eval time
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc, eval_time = 2))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 2))
  expect_snapshot(last_fit(wflow, split, metrics = met_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 2))
  expect_snapshot(last_fit(wflow, split, metrics = met_stc_int, eval_time = 2), error = TRUE)

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 2))
  expect_snapshot(last_fit(wflow, split, metrics = met_dyn_int, eval_time = 2), error = TRUE)

  expect_snapshot(last_fit(wflow, split, metrics = met_int_stc, eval_time = 2), error = TRUE)
  expect_snapshot(last_fit(wflow, split, metrics = met_int_dyn, eval_time = 2), error = TRUE)

  # multiple eval times
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_int, eval_time = 1:3))

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc_dyn, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_stc_int, eval_time = 1:3))

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn_stc, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_dyn_int, eval_time = 1:3))

  expect_snapshot(res <- last_fit(wflow, split, metrics = met_int_stc, eval_time = 1:3))
  expect_snapshot(res <- last_fit(wflow, split, metrics = met_int_dyn, eval_time = 1:3))


})

