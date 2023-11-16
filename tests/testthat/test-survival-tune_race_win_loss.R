suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

skip_if_not_installed("finetune", minimum_version = "1.1.0.9001")
skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.1.9001")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
skip_if_not_installed("finetune", minimum_version = "1.1.0.9001")

test_that("race tuning (win_loss) survival models with static metric", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- bootstraps(sim_tr, times = 20)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid <- tibble(cost_complexity = 10^c(-10, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with static metrics -------------------------------------------------

  stc_mtrc  <- metric_set(concordance_survival)

  set.seed(2193)
  wl_static_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = stc_mtrc,
      control = rctrl
    )

  num_final_wl  <- unique(show_best(wl_static_res)$cost_complexity)

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(wl_static_res$.metrics[[1]]))

  expect_equal(
    names(wl_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "cost_complexity", "event_time", ".config")
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(plot_race(wl_static_res)),
    "stc-wl-race-plot"
  )

  if (length(num_final_wl) > 1) {
    expect_snapshot_plot(
      print(autoplot(wl_static_res)),
      "stc-wl-race-2-times"
    )
  }

  # test metric collection -----------------------------------------------------

   exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )
  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  ###

  wl_finished <-
    map_dfr(wl_static_res$.metrics, I) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_static_res)

  expect_equal(nrow(wl_finished), nrow(metric_wl_sum))
  expect_equal(metric_wl_sum[0,], exp_metric_sum)
  expect_true(all(metric_wl_sum$.metric == "concordance_survival"))

  ###

  metric_wl_all <- collect_metrics(wl_static_res, summarize = FALSE)
  expect_true(nrow(metric_wl_all) == nrow(wl_finished) * nrow(sim_rs))
  expect_equal(metric_wl_all[0,], exp_metric_all)
  expect_true(all(metric_wl_all$.metric == "concordance_survival"))

})

test_that("race tuning (win_loss) survival models with integrated metric", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- bootstraps(sim_tr, times = 6)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  # make it so there will probably be 2+ winners
  grid <- tibble(cost_complexity = 10^c(-10.1, -10.0, -2.0, -1.0))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with integrated metrics ---------------------------------------------

  sint_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  wl_integrated_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  num_final_wl  <- unique(show_best(wl_integrated_res, eval_time = 5)$cost_complexity)

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(wl_integrated_res$.metrics[[1]]))

  expect_equal(
    names(wl_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "cost_complexity", "event_time", ".config")
  )

  expect_true(is.list(wl_integrated_res$.predictions[[1]]$.pred))

  expect_equal(
    names(wl_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    wl_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(plot_race(wl_integrated_res)),
    "int-wl-race-plot"
  )

  if (length(num_final_wl) > 1) {
    expect_snapshot_plot(
      print(autoplot(wl_integrated_res)),
      "int-wl-racing"
    )
  }

  # test metric collection

  exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )
  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  ###

  wl_finished <-
    map_dfr(wl_integrated_res$.metrics, I) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_integrated_res)

  expect_equal(nrow(wl_finished), nrow(metric_wl_sum))
  expect_equal(metric_wl_sum[0,], exp_metric_sum)
  expect_true(all(metric_wl_sum$.metric == "brier_survival_integrated"))

  ###

  metric_wl_all <- collect_metrics(wl_integrated_res, summarize = FALSE)
  expect_true(nrow(metric_wl_all) == nrow(wl_finished) * nrow(sim_rs))
  expect_equal(metric_wl_all[0,], exp_metric_all)
  expect_true(all(metric_wl_all$.metric == "brier_survival_integrated"))

})

test_that("race tuning (win_loss) survival models with dynamic metrics", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- bootstraps(sim_tr, times = 20)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid <- tibble(cost_complexity = 10^c(-10, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with dynamic metrics ------------------------------------------------

  dyn_mtrc  <- metric_set(brier_survival)

  set.seed(2193)
  wl_dyn_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  num_final_wl  <- unique(show_best(wl_dyn_res, eval_time = 5)$cost_complexity)

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(wl_dyn_res$.metrics[[1]]))

  expect_equal(
    names(wl_dyn_res$.predictions[[1]]),
    c(".pred", ".row", "cost_complexity", "event_time", ".config")
  )

  expect_true(is.list(wl_dyn_res$.predictions[[1]]$.pred))

  expect_equal(
    names(wl_dyn_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    wl_dyn_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(plot_race(wl_dyn_res)),
    "dyn-wl-race-plot"
  )

  expect_snapshot_plot(
    print(autoplot(wl_dyn_res, eval_time = c(1, 5))),
    "dyn-wl-race-2-times"
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(wl_dyn_res)),
      "dyn-wl-race-0-times"
    )
  )

  # test metric collection -----------------------------------------------------

    exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  ###

  wl_finished <-
    map_dfr(wl_dyn_res$.metrics, I) %>%
    filter(.eval_time == 5) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_dyn_res)

  expect_equal(nrow(wl_finished) * length(time_points), nrow(metric_wl_sum))
  expect_equal(metric_wl_sum[0,], exp_metric_sum)
  expect_true(all(metric_wl_sum$.metric == "brier_survival"))

  ###

  metric_wl_all <- collect_metrics(wl_dyn_res, summarize = FALSE)
  expect_true(nrow(metric_wl_all) == nrow(wl_finished) * nrow(sim_rs) * length(time_points))
  expect_equal(metric_wl_all[0,], exp_metric_all)
  expect_true(all(metric_wl_all$.metric == "brier_survival"))

})

test_that("race tuning (win_loss) survival models with mixture of metric types", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- bootstraps(sim_tr, times = 30)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid_winner <- tibble(cost_complexity = 10^c(-10, seq(-1.1, -1, length.out = 5)))
  grid_ties <- tibble(cost_complexity = 10^c(seq(-10.1, -10.0, length.out = 5)))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with mixed metrics --------------------------------------------------

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  set.seed(2193)
  wl_mixed_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid_ties,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  num_final_wl  <- unique(show_best(wl_mixed_res, metric = "brier_survival", eval_time = 5)$cost_complexity)

  expect_equal(length(num_final_wl), nrow(grid_ties))

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(wl_mixed_res$.metrics[[1]]))

  expect_equal(
    names(wl_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "cost_complexity", ".pred_time", "event_time", ".config")
  )

  expect_true(is.list(wl_mixed_res$.predictions[[1]]$.pred))

  expect_equal(
    names(wl_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    wl_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test autoplot --------------------------------------------------------------

  expect_snapshot_plot(
    print(plot_race(wl_mixed_res)),
    "wl-race-plot"
  )

  # TODO make better plot at resolution of https://github.com/tidymodels/tune/issues/754
  # expect_snapshot_plot(
  #   print(autoplot(aov_mixed_res, eval_time = c(1, 5))),
  #   "mix-aov-race-2-times"
  # )
  expect_snapshot_plot(
    print(autoplot(wl_mixed_res, eval_time = c(1, 5))),
    "mix-wl-race-2-times"
  )
  expect_snapshot_plot(
    print(autoplot(wl_mixed_res, metric = "concordance_survival")),
    "mix-wl-race-1-metric"
  )

  # TODO make better plot at resolution of https://github.com/tidymodels/tune/issues/754
  # expect_snapshot_warning(
  #   expect_snapshot_plot(
  #     print(autoplot(aov_mixed_res)),
  #     "mix-aov-race-0-times"
  #   )
  # )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(wl_mixed_res)),
      "mix-wl-race-0-times"
    )
  )

  # test metric collection -----------------------------------------------------

    exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0)
  )
  num_metrics <- length(time_points) + 2

  ###

  wl_finished <-
    map_dfr(wl_mixed_res$.metrics, I) %>%
    filter(.eval_time == 5) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_mixed_res)

  expect_equal(nrow(wl_finished) * num_metrics, nrow(metric_wl_sum))
  expect_equal(metric_wl_sum[0,], exp_metric_sum)
  expect_true(sum(is.na(metric_wl_sum$.eval_time)) == 2 * nrow(wl_finished))
  expect_equal(as.vector(table(metric_wl_sum$.metric)), c(4L, 1L, 1L) * nrow(wl_finished))

  ###

  metric_wl_all <- collect_metrics(wl_mixed_res, summarize = FALSE)
  expect_true(nrow(metric_wl_all) == num_metrics * nrow(wl_finished) * nrow(sim_rs))
  expect_equal(metric_wl_all[0,], exp_metric_all)
  expect_true(sum(is.na(metric_wl_sum$.eval_time)) == 2 * nrow(wl_finished))
  expect_equal(as.vector(table(metric_wl_sum$.metric)), c(4L, 1L, 1L) * nrow(wl_finished))

  # test show_best() -----------------------------------------------------------

  # TODO make these use wl_mixed_res instead (has not yet been tested)
  expect_snapshot_warning(show_best(aov_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(aov_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1.001)),
    error = TRUE
  )
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1, 3)),
    error = TRUE
  )
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival_integrated")
  )
})
