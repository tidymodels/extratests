suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

skip_if_not_installed("finetune", minimum_version = "1.1.0.9001")
skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.1.9001")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
skip_if_not_installed("finetune", minimum_version = "1.1.0.9001")

test_that("sim annealing tuning survival models with static metric", {
  skip_if_not_installed("mboost")
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

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
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  mod_param <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(1, 50)))

  grid <- tibble(trees = c(1, 5, 20))

  gctrl <- control_grid(save_pred = TRUE)
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)

  # Simulated annealing with static metrics ------------------------------------

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
  sa_static_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      param_info = mod_param,
      metrics = stc_mtrc,
      control = sctrl,
      initial = init_grid_static_res
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(sa_static_res$.metrics[[1]]))
  expect_equal(
    names(sa_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "trees", "event_time", ".config")
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(sa_static_res)),
    "stc-sa"
  )

  expect_snapshot_plot(
    print(autoplot(sa_static_res, type = "parameters")),
    "stc-sa-param"
  )
  expect_snapshot_plot(
    print(autoplot(sa_static_res, type = "performance")),
    "stc-sa-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(sa_static_res)
  exp_metric_sum <- tibble(
    trees = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_sum) == 5)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "concordance_survival"))

  metric_all <- collect_metrics(sa_static_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    trees = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_all) == 50)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "concordance_survival"))

})

test_that("sim annealing tuning survival models with integrated metric", {
  skip_if_not_installed("mboost")
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

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
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  mod_param <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(1, 50)))

  grid <- tibble(trees = c(1, 5, 20))

  gctrl <- control_grid(save_pred = TRUE)
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)

  # Simulated annealing with integrated metrics --------------------------------

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
  sa_integrated_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      param_info = mod_param,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = sctrl,
      initial = init_grid_integrated_res
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(sa_integrated_res$.metrics[[1]]))
  expect_equal(
    names(sa_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "trees", "event_time", ".config")
  )
  expect_true(is.list(sa_integrated_res$.predictions[[1]]$.pred))
  expect_equal(
    names(sa_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    sa_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(sa_integrated_res)),
    "int-sa"
  )
  expect_snapshot_plot(
    print(autoplot(sa_integrated_res, type = "parameters")),
    "int-sa-param"
  )
  expect_snapshot_plot(
    print(autoplot(sa_integrated_res, type = "performance")),
    "int-sa-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(sa_integrated_res)
  exp_metric_sum <- tibble(
    trees = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_sum) == 5)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival_integrated"))

  metric_all <- collect_metrics(sa_integrated_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    trees = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_all) == 50)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival_integrated"))

})

test_that("sim annealing tuning survival models with dynamic metric", {
  skip_if_not_installed("mboost")
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

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
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  mod_param <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(1, 50)))

  grid <- tibble(trees = c(1, 5, 20))

  gctrl <- control_grid(save_pred = TRUE)
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)

  # Simulated annealing with a dynamic metric ----------------------------------

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
  sa_dynamic_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      param_info = mod_param,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = sctrl,
      initial = init_grid_dynamic_res
    )

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(sa_dynamic_res$.metrics[[1]]))
  expect_equal(
    names(sa_dynamic_res$.predictions[[1]]),
    c(".pred", ".row", "trees", "event_time", ".config")
  )
  expect_true(is.list(sa_dynamic_res$.predictions[[1]]$.pred))
  expect_equal(
    names(sa_dynamic_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    sa_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(sa_dynamic_res)),
      "dyn-sa"
    )
  )
  expect_snapshot_plot(
    print(autoplot(sa_dynamic_res, eval_time = 1, type = "parameters")),
    "dyn-sa-param"
  )
  expect_snapshot_plot(
    print(autoplot(sa_dynamic_res, eval_time = 10, type = "performance")),
    "dyn-sa-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(sa_dynamic_res)
  exp_metric_sum <- tibble(
    trees = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_sum) == 14)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival"))

  metric_all <- collect_metrics(sa_dynamic_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    trees = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_all) == 140)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival"))

})

test_that("sim annealing tuning survival models with mixture of metric types", {
  skip_if_not_installed("mboost")
  skip_if_not_installed("prodlim")

  # standard setup start -------------------------------------------------------

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
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  mod_param <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(1, 50)))

  grid <- tibble(trees = c(1, 5, 20))

  gctrl <- control_grid(save_pred = TRUE)
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)

  # Simulated annealing with a mixture of metrics ------------------------------

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
  sa_mixed_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      iter = 2,
      param_info = mod_param,
      metrics = mix_mtrc,
      eval_time = time_points,
      initial = init_grid_mixed_res,
      control = sctrl
    )

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(sa_mixed_res$.metrics[[1]]))
  expect_equal(
    names(sa_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "trees", ".pred_time", "event_time", ".config")
  )
  expect_true(is.list(sa_mixed_res$.predictions[[1]]$.pred))
  expect_equal(
    names(sa_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    sa_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(sa_mixed_res, eval_time = c(1, 5))),
    "mix-sa-2-times"
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(sa_mixed_res)),
      "mix-sa-0-times"
    )
  )
  expect_snapshot_plot(
    print(autoplot(sa_mixed_res, eval_time = 1, type = "parameters")),
    "mix-sa-param"
  )
  expect_snapshot_plot(
    print(autoplot(sa_mixed_res, eval_time = 10, type = "performance")),
    "mix-sa-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(sa_mixed_res)
  exp_metric_sum <- tibble(
    trees = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_sum) == 24L)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(sum(is.na(metric_sum$.eval_time)) == 10L)
  expect_equal(as.vector(table(metric_sum$.metric)), c(14L, 5L, 5L))

  metric_all <- collect_metrics(sa_mixed_res, summarize = FALSE)
  exp_metric_all <- tibble(
      id = character(0),
      trees = numeric(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0),
      .iter = integer(0)
    )

  expect_true(nrow(metric_all) == 240L)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(sum(is.na(metric_all$.eval_time)) == 100L)
  expect_equal(as.vector(table(metric_all$.metric)), c(140L, 50L, 50L))

  # test show_best() -----------------------------------------------------------

  expect_snapshot_warning(show_best(sa_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(sa_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot(
    show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1.001)),
    error = TRUE
  )
  expect_snapshot(
    show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1, 3)),
    error = TRUE
  )
  expect_snapshot(
    show_best(sa_mixed_res, metric = "brier_survival_integrated")
  )
})
