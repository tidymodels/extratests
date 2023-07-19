library(testthat)
library(tidymodels)
library(censored)
library(yardstick)
library(finetune)

skip_if_not_installed("finetune", minimum_version = "1.1.0.9001")

test_that("race tuning survival models with static metric", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("coin") # required for partykit engine

  stc_mtrc  <- metric_set(concordance_survival)

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

  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)
  # standard setup end

  set.seed(2193)
  aov_static_res <-
    mod_spec %>%
    tune_race_anova(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = stc_mtrc,
      control = rctrl
    )

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

  expect_false(".eval_time" %in% names(aov_static_res$.metrics[[1]]))
  expect_false(".eval_time" %in% names(wl_static_res$.metrics[[1]]))

  expect_equal(
    names(aov_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "tree_depth", "event_time", ".config")
  )
  expect_equal(
    names(wl_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "tree_depth", "event_time", ".config")
  )

  expect_snapshot_plot(
    print(plot_race(aov_static_res)),
    "static-aov-racing-plot"
  )
  expect_snapshot_plot(
    print(plot_race(wl_static_res)),
    "static-wl-racing-plot"
  )

  expect_snapshot_plot(
    print(autoplot(aov_static_res)),
    "static-metric-aov-racing-with-two-time-points"
  )
  expect_snapshot_plot(
    print(autoplot(wl_static_res)),
    "static-metric-wl-racing-with-two-time-points"
  )

  expect_snapshot_plot(
    print(autoplot(aov_static_res, type = "marginals")),
    "static-metric-aov-racing-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(wl_static_res, type = "marginals")),
    "static-metric-wl-racing-with-two-time-points-marginals"
  )
})

test_that("race tuning survival models with integrated metric", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("coin") # required for partykit engine

  sint_mtrc <- metric_set(brier_survival_integrated)

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

  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)
  # standard setup end

  set.seed(2193)
  aov_integrated_res <-
    mod_spec %>%
    tune_race_anova(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = rctrl
    )

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

  expect_false(".eval_time" %in% names(aov_integrated_res$.metrics[[1]]))
  expect_false(".eval_time" %in% names(wl_integrated_res$.metrics[[1]]))

  expect_equal(
    names(aov_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_equal(
    names(wl_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )

  expect_true(is.list(aov_integrated_res$.predictions[[1]]$.pred))
  expect_true(is.list(wl_integrated_res$.predictions[[1]]$.pred))

  expect_equal(
    names(aov_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    names(wl_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    aov_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )
  expect_equal(
    wl_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_plot(
    print(plot_race(aov_integrated_res)),
    "integrated-metric-aov-racing-plot"
  )
  expect_snapshot_plot(
    print(plot_race(wl_integrated_res)),
    "integrated-metric-wl-racing-plot"
  )

  expect_snapshot_plot(
    print(autoplot(aov_integrated_res, eval_time = c(1, 5))),
    "integrated-metric-aov-racing-with-two-time-points"
  )
  expect_snapshot_plot(
    print(autoplot(wl_integrated_res, eval_time = c(1, 5))),
    "integrated-metric-wl-racing-with-two-time-points"
  )

  expect_snapshot_plot(
    print(autoplot(aov_integrated_res)),
    "integrated-metric-aov-racing-with-no-set-time-points"
  )
  expect_snapshot_plot(
    print(autoplot(wl_integrated_res)),
    "integrated-metric-wl-racing-with-no-set-time-points"
  )

  expect_snapshot_plot(
    print(autoplot(aov_integrated_res, eval_time = 1, type = "marginals")),
    "integrated-metric-aov-racing-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(wl_integrated_res, eval_time = 1, type = "marginals")),
    "integrated-metric-wl-racing-with-two-time-points-marginals"
  )
})

test_that("race tuning survival models with dynamic metrics", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("coin") # required for partykit engine

  dyn_mtrc  <- metric_set(brier_survival)

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

  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)
  # standard setup end

  set.seed(2193)
  aov_dyn_res <-
    mod_spec %>%
    tune_race_anova(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = rctrl
    )

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

  expect_true(".eval_time" %in% names(aov_dyn_res$.metrics[[1]]))
  expect_true(".eval_time" %in% names(wl_dyn_res$.metrics[[1]]))

  expect_equal(
    names(aov_dyn_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )
  expect_equal(
    names(wl_dyn_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", "event_time", ".config")
  )

  expect_true(is.list(aov_dyn_res$.predictions[[1]]$.pred))
  expect_true(is.list(wl_dyn_res$.predictions[[1]]$.pred))

  expect_equal(
    names(aov_dyn_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    names(wl_dyn_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    aov_dyn_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )
  expect_equal(
    wl_dyn_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_plot(
    print(plot_race(aov_dyn_res)),
    "dyn-aov-racing-plot"
  )
  expect_snapshot_plot(
    print(plot_race(wl_dyn_res)),
    "dyn-wl-racing-plot"
  )

  expect_snapshot_plot(
    print(autoplot(aov_dyn_res, eval_time = c(1, 5))),
    "dyn-metric-aov-racing-with-two-time-points"
  )
  expect_snapshot_plot(
    print(autoplot(wl_dyn_res, eval_time = c(1, 5))),
    "dyn-metric-wl-racing-with-two-time-points"
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(aov_dyn_res)),
      "dyn-metric-aov-racing-with-no-set-time-points"
    )
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(wl_dyn_res)),
      "dyn-metric-wl-racing-with-no-set-time-points"
    )
  )

  expect_snapshot_plot(
    print(autoplot(aov_dyn_res, eval_time = 1, type = "marginals")),
    "dyn-metric-aov-racing-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(wl_dyn_res, eval_time = 1, type = "marginals")),
    "dyn-metric-wl-racing-with-two-time-points-marginals"
  )
})

test_that("race tuning survival models with mixture of metric types", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("coin") # required for partykit engine

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

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

  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)
  # standard setup end

  set.seed(2193)
  aov_mixed_res <-
    mod_spec %>%
    tune_race_anova(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  set.seed(2193)
  wl_mixed_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  expect_true(".eval_time" %in% names(aov_mixed_res$.metrics[[1]]))
  expect_true(".eval_time" %in% names(wl_mixed_res$.metrics[[1]]))

  expect_equal(
    names(aov_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", ".pred_time", "event_time", ".config")
  )
  expect_equal(
    names(wl_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "tree_depth", ".pred_time", "event_time", ".config")
  )

  expect_true(is.list(aov_mixed_res$.predictions[[1]]$.pred))
  expect_true(is.list(wl_mixed_res$.predictions[[1]]$.pred))

  expect_equal(
    names(aov_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )
  expect_equal(
    names(wl_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    aov_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )
  expect_equal(
    wl_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  expect_snapshot_plot(
    print(plot_race(aov_mixed_res)),
    "aov-racing-plot"
  )
  expect_snapshot_plot(
    print(plot_race(wl_mixed_res)),
    "wl-racing-plot"
  )

  expect_snapshot_plot(
    print(autoplot(aov_mixed_res, eval_time = c(1, 5))),
    "mixed-metric-aov-racing-with-two-time-points"
  )
  expect_snapshot_plot(
    print(autoplot(wl_mixed_res, eval_time = c(1, 5))),
    "mixed-metric-wl-racing-with-two-time-points"
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(aov_mixed_res)),
      "mixed-metric-aov-racing-with-no-set-time-points"
    )
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      print(autoplot(wl_mixed_res)),
      "mixed-metric-wl-racing-with-no-set-time-points"
    )
  )

  expect_snapshot_plot(
    print(autoplot(aov_mixed_res, eval_time = 1, type = "marginals")),
    "mixed-metric-aov-racing-with-two-time-points-marginals"
  )
  expect_snapshot_plot(
    print(autoplot(wl_mixed_res, eval_time = 1, type = "marginals")),
    "mixed-metric-wl-racing-with-two-time-points-marginals"
  )

  # test some S3 methods for any tune_result object
  expect_snapshot_warning(show_best(aov_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(aov_mixed_res, metric = "brier_survival", eval_time = 1))
  expect_snapshot_error(
    show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1.001))
  )
  expect_snapshot_error(
    show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival_integrated")
  )
})
