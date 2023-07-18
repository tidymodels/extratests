library(testthat)
library(tidymodels)
library(prodlim)
library(censored)
library(yardstick)
library(finetune)

# ------------------------------------------------------------------------------

test_that('race tuning survival models ', {
  skip_if_not_installed("coin") # required for partykit engine

  set.seed(1)
  sim_dat <- SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  dyn_mtrc  <- metric_set(brier_survival)
  stc_mtrc  <- metric_set(concordance_survival)
  sint_mtrc <- metric_set(brier_survival_integrated)
  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  mod_spec <-
    decision_tree(tree_depth = tune(), min_n = 4) %>%
    set_engine("partykit") %>%
    set_mode("censored regression")

  grid <- tibble(tree_depth = c(1, 2, 10))

  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # ------------------------------------------------------------------------------
  # Mixtures of static, dynamic, and integrated metrics

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
    "aov-racing-plot",
    print(plot_race(aov_mixed_res))
  )
  expect_snapshot_plot(
    "wl-racing-plot",
    print(plot_race(wl_mixed_res))
  )

  expect_snapshot_plot(
    "mixed-metric-aov-racing-with-two-time-points",
    print(autoplot(aov_mixed_res, eval_time = c(1, 5)))
  )
  expect_snapshot_plot(
    "mixed-metric-wl-racing-with-two-time-points",
    print(autoplot(wl_mixed_res, eval_time = c(1, 5)))
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      "mixed-metric-aov-racing-with-no-set-time-points",
      print(autoplot(aov_mixed_res))
    )
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      "mixed-metric-wl-racing-with-no-set-time-points",
      print(autoplot(wl_mixed_res))
    )
  )

  expect_snapshot_plot(
    "mixed-metric-aov-racing-with-two-time-points-marginals",
    print(autoplot(aov_mixed_res, eval_time = 1, type = "marginals"))
  )
  expect_snapshot_plot(
    "mixed-metric-wl-racing-with-two-time-points-marginals",
    print(autoplot(wl_mixed_res, eval_time = 1, type = "marginals"))
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

  # ------------------------------------------------------------------------------
  # Static metrics only

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
    "static-aov-racing-plot",
    print(plot_race(aov_static_res))
  )
  expect_snapshot_plot(
    "static-wl-racing-plot",
    print(plot_race(wl_static_res))
  )

  expect_snapshot_plot(
    "static-metric-aov-racing-with-two-time-points",
    print(autoplot(aov_static_res))
  )
  expect_snapshot_plot(
    "static-metric-wl-racing-with-two-time-points",
    print(autoplot(wl_static_res))
  )

  expect_snapshot_plot(
    "static-metric-aov-racing-with-two-time-points-marginals",
    print(autoplot(aov_static_res, type = "marginals"))
  )
  expect_snapshot_plot(
    "static-metric-wl-racing-with-two-time-points-marginals",
    print(autoplot(wl_static_res, type = "marginals"))
  )

  # ------------------------------------------------------------------------------
  # Dynamic metrics only

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
    "dyn-aov-racing-plot",
    print(plot_race(aov_dyn_res))
  )
  expect_snapshot_plot(
    "dyn-wl-racing-plot",
    print(plot_race(wl_dyn_res))
  )

  expect_snapshot_plot(
    "dyn-metric-aov-racing-with-two-time-points",
    print(autoplot(aov_dyn_res, eval_time = c(1, 5)))
  )
  expect_snapshot_plot(
    "dyn-metric-wl-racing-with-two-time-points",
    print(autoplot(wl_dyn_res, eval_time = c(1, 5)))
  )

  expect_snapshot_warning(
    expect_snapshot_plot(
      "dyn-metric-aov-racing-with-no-set-time-points",
      print(autoplot(aov_dyn_res))
    )
  )
  expect_snapshot_warning(
    expect_snapshot_plot(
      "dyn-metric-wl-racing-with-no-set-time-points",
      print(autoplot(wl_dyn_res))
    )
  )

  expect_snapshot_plot(
    "dyn-metric-aov-racing-with-two-time-points-marginals",
    print(autoplot(aov_dyn_res, eval_time = 1, type = "marginals"))
  )
  expect_snapshot_plot(
    "dyn-metric-wl-racing-with-two-time-points-marginals",
    print(autoplot(wl_dyn_res, eval_time = 1, type = "marginals"))
  )

})
