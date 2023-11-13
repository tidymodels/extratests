suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.1.9001")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("Bayesian tuning survival models with static metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Bayes with static metrics

  stc_mtrc  <- metric_set(concordance_survival)

  set.seed(2193)
  init_grid_static_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = stc_mtrc,
      control = gctrl
    )

  set.seed(2193)
  bayes_static_res <-
    mod_spec %>%
    tune_bayes(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = stc_mtrc,
      control = bctrl,
      initial = init_grid_static_res
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_false(".eval_time" %in% names(bayes_static_res$.metrics[[1]]))
  expect_equal(
    names(bayes_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "tree_depth", "event_time", ".config")
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(bayes_static_res)),
    "static-metric-bayes-search"
  )

  expect_snapshot_plot(
    print(autoplot(bayes_static_res, type = "marginals")),
    "static-bayes-search-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_static_res, type = "performance")),
    "static-bayes-search-with-two-time-points-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection

  metric_sum <- collect_metrics(bayes_static_res)
  exp_metric_sum <-
    structure(
      list(
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 5)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "concordance_survival"))

  metric_all <- collect_metrics(bayes_static_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .estimate = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 50)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "concordance_survival"))
})

test_that("Bayesian tuning survival models with integrated metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)
  # standard setup end
  # ------------------------------------------------------------------------------
  # Bayes with integrated metric

  sint_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  init_grid_integrated_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  set.seed(2193)
  bayes_integrated_res <-
    mod_spec %>%
    tune_bayes(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = bctrl,
      initial = init_grid_integrated_res
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_false(".eval_time" %in% names(bayes_integrated_res$.metrics[[1]]))
  expect_equal(
    names(bayes_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_true(is.list(bayes_integrated_res$.predictions[[1]]$.pred))
  expect_equal(
    names(bayes_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    bayes_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(bayes_integrated_res)),
    "integrated-metric-bayes-search"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_integrated_res, type = "marginals")),
    "integrated-metric-bayes-search-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_integrated_res, type = "performance")),
    "integrated-metric-bayes-search-with-two-time-points-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection

  metric_sum <- collect_metrics(bayes_integrated_res)
  exp_metric_sum <-
    structure(
      list(
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 5)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival_integrated"))

  metric_all <- collect_metrics(bayes_integrated_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .estimate = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 50)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival_integrated"))
})

test_that("Bayesian tuning survival models with dynamic metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)
  # standard setup end
  # ------------------------------------------------------------------------------
  # Bayes with dynamic metric

  dyn_mtrc  <- metric_set(brier_survival)

  set.seed(2193)
  init_grid_dynamic_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  set.seed(2193)
  bayes_dynamic_res <-
    mod_spec %>%
    tune_bayes(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = bctrl,
      initial = init_grid_dynamic_res
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_true(".eval_time" %in% names(bayes_dynamic_res$.metrics[[1]]))
  expect_equal(
    names(bayes_dynamic_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_true(is.list(bayes_dynamic_res$.predictions[[1]]$.pred))
  expect_equal(
    names(bayes_dynamic_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    bayes_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(bayes_dynamic_res)),
      "dynamic-metric-bayes-search"
    )
  )
  expect_snapshot_plot(
    print(autoplot(bayes_dynamic_res, eval_time = 1, type = "marginals")),
    "dynamic-metric-bayes-search-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_dynamic_res, eval_time = 1, type = "performance")),
    "dynamic-metric-bayes-search-with-two-time-points-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection

  metric_sum <- collect_metrics(bayes_dynamic_res)
  exp_metric_sum <-
    structure(
      list(
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 20)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival"))

  metric_all <- collect_metrics(bayes_dynamic_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        .estimate = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 200)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival"))
})

test_that("Bayesian tuning survival models with mixture of metric types", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)
  # standard setup end
  # ------------------------------------------------------------------------------
  # Bayes with a mixture of all three types

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  set.seed(2193)
  init_grid_mixed_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  set.seed(2193)
  bayes_mixed_res <-
    mod_spec %>%
    tune_bayes(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      metrics = mix_mtrc,
      eval_time = time_points,
      initial = init_grid_mixed_res,
      control = bctrl
    )

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_true(".eval_time" %in% names(bayes_mixed_res$.metrics[[1]]))
  expect_equal(
    names(bayes_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", ".pred_time", "event_time", ".config")
  )
  expect_true(is.list(bayes_mixed_res$.predictions[[1]]$.pred))
  expect_equal(
    names(bayes_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    bayes_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(bayes_mixed_res, eval_time = c(1, 5))),
    "mixed-metric-bayes-search-with-two-time-points"
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(bayes_mixed_res)),
      "mixed-metric-bayes-search-with-no-set-time-points"
    )
  )
  expect_snapshot_plot(
    print(autoplot(bayes_mixed_res, eval_time = 1, type = "marginals")),
    "mixed-metric-bayes-search-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_mixed_res, eval_time = 1, type = "performance")),
    "mixed-metric-bayes-search-with-two-time-points-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection

  metric_sum <- collect_metrics(bayes_mixed_res)
  exp_metric_sum <-
    structure(
      list(
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        mean = numeric(0),
        n = integer(0),
        std_err = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_true(nrow(metric_sum) == 30)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(sum(is.na(metric_sum$.eval_time)) == 10)
  expect_equal(as.vector(table(metric_sum$.metric)), c(20L, 5L, 5L))

  metric_all <- collect_metrics(bayes_mixed_res, summarize = FALSE)
  exp_metric_all <-
    structure(
      list(
        id = character(0),
        tree_depth = numeric(0),
        .metric = character(0),
        .estimator = character(0),
        .eval_time = numeric(0),
        .estimate = numeric(0),
        .config = character(0),
        .iter = integer(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_true(nrow(metric_all) == 300)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(sum(is.na(metric_all$.eval_time)) == 100)
  expect_equal(as.vector(table(metric_all$.metric)), c(200L, 50L, 50L))

  # ------------------------------------------------------------------------------
  # test show/select methods

  expect_snapshot_warning(show_best(bayes_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(bayes_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot_error(
    show_best(bayes_mixed_res, metric = "brier_survival", eval_time = c(1.001))
  )
  expect_snapshot_error(
    show_best(bayes_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(bayes_mixed_res, metric = "brier_survival_integrated")
  )

})
