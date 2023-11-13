suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.1.9001")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("grid tuning survival models with static metric", {
  skip_if_not_installed("prodlim")

  # ------------------------------------------------------------------------------
  # standard setup start

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Grid search with static metrics

  stc_mtrc  <- metric_set(concordance_survival)

  set.seed(2193)
  grid_static_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = stc_mtrc,
      control = gctrl
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_false(".eval_time" %in% names(grid_static_res$.metrics[[1]]))
  expect_equal(
    names(grid_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "penalty", "event_time", ".config")
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(grid_static_res)),
    "static-metric-grid-search"
  )

  # ------------------------------------------------------------------------------
  # test metric collection

  metric_sum <- collect_metrics(grid_static_res)
  exp_metric_sum <-
    structure(
      list(
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 3)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "concordance_survival"))

  metric_all <- collect_metrics(grid_static_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .estimate = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 30)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "concordance_survival"))

})

test_that("grid tuning survival models with integrated metric", {
  skip_if_not_installed("prodlim")

  # ------------------------------------------------------------------------------
  # standard setup start

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Grid search with integrated metrics

  sint_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  grid_integrated_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_false(".eval_time" %in% names(grid_integrated_res$.metrics[[1]]))
  expect_equal(
    names(grid_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "penalty", "event_time", ".config")
  )
  expect_true(is.list(grid_integrated_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(grid_integrated_res)),
    "integrated-metric-grid-search"
  )

  # ------------------------------------------------------------------------------
  # test metrics collection

  metric_sum <- collect_metrics(grid_integrated_res)
  exp_metric_sum <-
    structure(
      list(
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 3)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival_integrated"))

  metric_all <- collect_metrics(grid_integrated_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .estimate = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 30)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival_integrated"))

})

test_that("grid tuning survival models with dynamic metric", {
  skip_if_not_installed("prodlim")

  # ------------------------------------------------------------------------------
  # standard setup start

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Grid search with dynamic metrics

  dyn_mtrc  <- metric_set(brier_survival)

  set.seed(2193)
  grid_dynamic_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_true(".eval_time" %in% names(grid_dynamic_res$.metrics[[1]]))
  expect_equal(
    names(grid_dynamic_res$.predictions[[1]]),
    c(".pred", ".row", "penalty", "event_time", ".config")
  )
  expect_true(is.list(grid_dynamic_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_dynamic_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(grid_dynamic_res)),
      "dynamic-metric-grid-search"
    )
  )

  expect_snapshot_plot(
    print(autoplot(grid_dynamic_res, eval_time = 1)),
    "dynamic-metric-grid-search_1"
  )

  # ------------------------------------------------------------------------------
  #  test metrics collection

  metric_sum <- collect_metrics(grid_dynamic_res)
  exp_metric_sum <-
    structure(
      list(
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 12)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival"))

  metric_all <- collect_metrics(grid_dynamic_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        .estimate = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 120)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival"))

})

test_that("grid tuning survival models mixture of metric types", {
  skip_if_not_installed("prodlim")

  # ------------------------------------------------------------------------------
  # standard setup start

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Grid search with a mixture of metrics

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  set.seed(2193)
  grid_mixed_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_true(".eval_time" %in% names(grid_mixed_res$.metrics[[1]]))
  expect_equal(
    names(grid_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "penalty", ".pred_time", "event_time", ".config")
  )
  expect_true(is.list(grid_mixed_res$.predictions[[1]]$.pred))
  expect_equal(
    names(grid_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    grid_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(grid_mixed_res, eval_time = c(1, 5))),
    "mixed-metric-grid-search-with-two-time-points"
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(grid_mixed_res)),
      "mixed-metric-grid-search-with-no-set-time-points"
    )
  )

  # ------------------------------------------------------------------------------
  # test metrics collection

  metric_sum <- collect_metrics(grid_mixed_res)
  exp_metric_sum <-
    structure(
      list(
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 18)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(sum(is.na(metric_sum$.eval_time)) == 6)
  expect_equal(as.vector(table(metric_sum$.metric)), c(12L, 3L, 3L))

  metric_all <- collect_metrics(grid_mixed_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        penalty = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        .estimate = numeric(0),
        .config = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 180)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(sum(is.na(metric_all$.eval_time)) == 60)
  expect_equal(as.vector(table(metric_all$.metric)), c(120L, 30L, 30L))

  # ------------------------------------------------------------------------------
  # test show/select methods

  expect_snapshot_warning(show_best(grid_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(grid_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot_error(
    show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1.001))
  )
  expect_snapshot_error(
    show_best(grid_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(grid_mixed_res, metric = "brier_survival_integrated")
  )
})
