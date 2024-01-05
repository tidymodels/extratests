suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.1.9001")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("Bayesian tuning survival models with static metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)

  # Bayes with static metrics --------------------------------------------------

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

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(bayes_static_res$.metrics[[1]]))
  expect_equal(
    names(bayes_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "tree_depth", "event_time", ".config")
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(bayes_static_res)),
    "stc-bayes"
  )

  expect_snapshot_plot(
    print(autoplot(bayes_static_res, type = "marginals")),
    "stc-bayes-2-times-marg"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_static_res, type = "performance")),
    "stc-bayes-2-times-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(bayes_static_res)
  exp_metric_sum <- tibble(
    tree_depth = numeric(0),
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

  metric_all <- collect_metrics(bayes_static_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    tree_depth = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_all) == 50)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "concordance_survival"))

  # test prediction collection -------------------------------------------------

  static_ptype <-
    structure(
      list(id = character(0), .pred_time = numeric(0), .row = integer(0),
           tree_depth = numeric(0),
           event_time = structure(numeric(0), type = "right", dim = c(0L, 2L),
                                  dimnames = list(NULL, c("time", "status")),
                                  class = "Surv"),
           .config = character(0), .iter = integer(0)), row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  unsum_pred <- collect_predictions(bayes_static_res)
  expect_equal(unsum_pred[0,], static_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  sum_pred <- collect_predictions(bayes_static_res, summarize = TRUE)
  expect_equal(sum_pred[0,], static_ptype[-1])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

})

test_that("Bayesian tuning survival models with integrated metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)

  # Bayes with integrated metric -----------------------------------------------

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

  # test structure of results --------------------------------------------------

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

  # test autoplot -------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(bayes_integrated_res)),
    "int-bayes"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_integrated_res, type = "marginals")),
    "int-bayes-2-times-marg"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_integrated_res, type = "performance")),
    "int-bayes-2-times-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(bayes_integrated_res)
  exp_metric_sum <- tibble(
    tree_depth = numeric(0),
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

  metric_all <- collect_metrics(bayes_integrated_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    tree_depth = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_all) == 50)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival_integrated"))

  # test prediction collection -------------------------------------------------

  integrated_ptype <-
    structure(
      list(id = character(0), .pred = list(), .row = integer(0),
           tree_depth = numeric(0),
           event_time = structure(numeric(0), type = "right", dim = c(0L, 2L),
                                  dimnames = list(NULL, c("time", "status")),
                                  class = "Surv"),
           .config = character(0), .iter = integer(0)), row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  integrated_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(bayes_integrated_res)
  expect_equal(unsum_pred[0,], integrated_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(unsum_pred$.pred[[1]][0,], integrated_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  # TODO fails to keep tuning parameter columns
  # sum_pred <- collect_predictions(bayes_integrated_res, summarize = TRUE)
  # expect_equal(sum_pred[0,], integrated_ptype[-1])
  # expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))
  #
  # expect_equal(sum_pred$.pred[[1]][0,], integrated_list_ptype)
  # expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))
})

test_that("Bayesian tuning survival models with dynamic metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)

  # Bayes with dynamic metric --------------------------------------------------

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

  # test structure of results --------------------------------------------------

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

  # test autoplot --------------------------------------------------------------

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(bayes_dynamic_res)),
      "dyn-bayes"
    )
  )
  expect_snapshot_plot(
    print(autoplot(bayes_dynamic_res, eval_time = 1, type = "marginals")),
    "dyn-bayes-2-times-marg"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_dynamic_res, eval_time = 1, type = "performance")),
    "dyn-bayes-2-times-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(bayes_dynamic_res)
  exp_metric_sum <- tibble(
    tree_depth = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_sum) == 20)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival"))

  metric_all <- collect_metrics(bayes_dynamic_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    tree_depth = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_all) == 200)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival"))

  # test prediction collection -------------------------------------------------

  dynamic_ptype <-
    structure(
      list(id = character(0), .pred = list(), .row = integer(0),
           tree_depth = numeric(0),
           event_time = structure(numeric(0), type = "right", dim = c(0L, 2L),
                                  dimnames = list(NULL, c("time", "status")),
                                  class = "Surv"),
           .config = character(0), .iter = integer(0)), row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  dynamic_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(bayes_dynamic_res)
  expect_equal(unsum_pred[0,], dynamic_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(unsum_pred$.pred[[1]][0,], dynamic_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  # sum_pred <- collect_predictions(bayes_dynamic_res, summarize = TRUE)
  # expect_equal(sum_pred[0,], dynamic_ptype[-1])
  # expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))
  #
  # expect_equal(sum_pred$.pred[[1]][0,], dynamic_list_ptype)
  # expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

})

test_that("Bayesian tuning survival models with mixture of metric types", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("coin") # required for partykit engine

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
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)

  # Bayes with a mixture of all three types ------------------------------------

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

  # test structure of results --------------------------------------------------

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

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(autoplot(bayes_mixed_res, eval_time = c(1, 5))),
    "mix-bayes-2-times"
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(bayes_mixed_res)),
      "mix-bayes-0-times"
    )
  )
  expect_snapshot_plot(
    print(autoplot(bayes_mixed_res, eval_time = 1, type = "marginals")),
    "mix-bayes-2-times-marg"
  )
  expect_snapshot_plot(
    print(autoplot(bayes_mixed_res, eval_time = 1, type = "performance")),
    "mix-bayes-2-times-perf"
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(bayes_mixed_res)
  exp_metric_sum <- tibble(
    tree_depth = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_sum) == 30)
  expect_equal(metric_sum[0,], exp_metric_sum)
  expect_true(sum(is.na(metric_sum$.eval_time)) == 10)
  expect_equal(as.vector(table(metric_sum$.metric)), c(20L, 5L, 5L))

  metric_all <- collect_metrics(bayes_mixed_res, summarize = FALSE)
  exp_metric_all <- tibble(
    id = character(0),
    tree_depth = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0),
    .iter = integer(0)
  )

  expect_true(nrow(metric_all) == 300)
  expect_equal(metric_all[0,], exp_metric_all)
  expect_true(sum(is.na(metric_all$.eval_time)) == 100)
  expect_equal(as.vector(table(metric_all$.metric)), c(200L, 50L, 50L))

  # test prediction collection -------------------------------------------------

  mixed_ptype <-
    structure(
      list(id = character(0), .pred = list(), .row = integer(0),
           tree_depth = numeric(0), .pred_time = numeric(0),
           event_time = structure(numeric(0), type = "right", dim = c(0L, 2L),
                                  dimnames = list(NULL, c("time", "status")),
                                  class = "Surv"),
           .config = character(0), .iter = integer(0)), row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"))

  mixed_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(bayes_mixed_res)
  expect_equal(unsum_pred[0,], mixed_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(unsum_pred$.pred[[1]][0,], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(bayes_mixed_res, summarize = TRUE)
  expect_equal(sum_pred[0,], mixed_ptype[-1])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * length(unique(unsum_pred$.config)))

  expect_equal(sum_pred$.pred[[1]][0,], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test show_best() -----------------------------------------------------------

  expect_snapshot_warning(show_best(bayes_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(bayes_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot(
    show_best(bayes_mixed_res, metric = "brier_survival", eval_time = c(1.001)),
    error = TRUE
  )
  # TODO no error
  # expect_snapshot(
  #   show_best(bayes_mixed_res, metric = "brier_survival", eval_time = c(1, 3)),
  #   error = TRUE
  # )
  expect_snapshot(
    show_best(bayes_mixed_res, metric = "brier_survival_integrated")
  )

})
