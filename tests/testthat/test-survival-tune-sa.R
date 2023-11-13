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
  skip_if_not_installed("mboost") # required for engine
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

  # standard setup end
  # ------------------------------------------------------------------------------
  # Simulated annealing with static metrics

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

  # ------------------------------------------------------------------------------
  # test structure of results

  expect_false(".eval_time" %in% names(sa_static_res$.metrics[[1]]))
  expect_equal(
    names(sa_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "trees", "event_time", ".config")
  )

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(sa_static_res)),
    "static-metric-sa-search"
  )

  expect_snapshot_plot(
    print(autoplot(sa_static_res, type = "parameters")),
    "static-sa-search-parameters"
  )
  expect_snapshot_plot(
    print(autoplot(sa_static_res, type = "performance")),
    "static-sa-search-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection

})

test_that("sim annealing tuning survival models with integrated metric", {
  skip_if_not_installed("mboost") # required for engine
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
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  mod_param <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(1, 50)))

  grid <- tibble(trees = c(10, 20))

  gctrl <- control_grid(save_pred = TRUE)
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Simulated annealing with integrated metrics

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

  # ------------------------------------------------------------------------------
  # test structure of results

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

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(sa_integrated_res)),
    "integrated-metric-sa-search"
  )
  expect_snapshot_plot(
    print(autoplot(sa_integrated_res, type = "parameters")),
    "integrated-metric-sa-search-parameters"
  )
  expect_snapshot_plot(
    print(autoplot(sa_integrated_res, type = "performance")),
    "integrated-metric-sa-search-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection



})

test_that("sim annealing tuning survival models with dynamic metric", {
  skip_if_not_installed("mboost") # required for engine
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
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  mod_param <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(1, 50)))

  grid <- tibble(trees = c(10, 20))

  gctrl <- control_grid(save_pred = TRUE)
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Simulated annealing with a dynamic metric

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

  # ------------------------------------------------------------------------------
  # test structure of results

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

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(sa_dynamic_res)),
      "dynamic-metric-sa-search"
    )
  )
  expect_snapshot_plot(
    print(autoplot(sa_dynamic_res, eval_time = 1, type = "parameters")),
    "dynamic-metric-sa-search-parameters"
  )
  expect_snapshot_plot(
    print(autoplot(sa_dynamic_res, eval_time = 10, type = "performance")),
    "dynamic-metric-sa-search-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection


})

test_that("sim annealing tuning survival models with mixture of metric types", {
  skip_if_not_installed("mboost") # required for engine
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
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  mod_param <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    update(trees = trees(c(1, 50)))

  grid <- tibble(trees = c(10, 20))

  gctrl <- control_grid(save_pred = TRUE)
  sctrl <- control_sim_anneal(save_pred = TRUE, verbose_iter = FALSE, verbose = FALSE)

  # standard setup end
  # ------------------------------------------------------------------------------
  # Simulated annealing with a mixture of metrics

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

  # ------------------------------------------------------------------------------
  # test structure of results

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

  # ------------------------------------------------------------------------------
  # test autoplot

  expect_snapshot_plot(
    print(autoplot(sa_mixed_res, eval_time = c(1, 5))),
    "mixed-metric-sa-search-with-two-time-points"
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(sa_mixed_res)),
      "mixed-metric-sa-search-with-no-set-time-points"
    )
  )
  expect_snapshot_plot(
    print(autoplot(sa_mixed_res, eval_time = 1, type = "parameters")),
    "mixed-metric-sa-search-parameters"
  )
  expect_snapshot_plot(
    print(autoplot(sa_mixed_res, eval_time = 10, type = "performance")),
    "mixed-metric-sa-search-performance"
  )

  # ------------------------------------------------------------------------------
  # test metric collection

  # ------------------------------------------------------------------------------
  # test show/select methods

  expect_snapshot_warning(show_best(sa_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(sa_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot_error(
    show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1.001))
  )
  expect_snapshot_error(
    show_best(sa_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(sa_mixed_res, metric = "brier_survival_integrated")
  )
})
