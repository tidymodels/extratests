suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

test_that("percentile internals for survival models with static metric", {
  skip_if_not_installed("prodlim")

  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
  skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9015")
  skip_if_not_installed("yardstick", minimum_version = "1.3.0")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr, v = 2)

  time_points <- c(10, 1, 5, 15)

  # last fit for models with static metrics ------------------------------------

  stc_mtrc <- metric_set(concordance_survival)

  set.seed(2193)
  rs_static_res <-
    survival_reg() %>%
    last_fit(
      event_time ~ X1 + X2,
      split = split,
      metrics = stc_mtrc
    )

  set.seed(1)
  static_int <- int_pctl(rs_static_res, times = 1001)

  exp_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .lower = numeric(0),
      .estimate = numeric(0),
      .upper = numeric(0),
      .config = character(0)
    )

  expect_ptype(static_int, exp_ptype)
  expect_true(nrow(static_int) == 1)
  expect_true(all(static_int$.metric == "concordance_survival"))

  # make sure `alpha` works
  set.seed(1)
  static_int_45 <- int_pctl(rs_static_res, times = 1001, alpha = 0.45)
  expect_true(static_int$.lower < static_int_45$.lower)
  expect_true(static_int$.upper > static_int_45$.upper)
  expect_equal(static_int$.estimate, static_int_45$.estimate)
})


test_that("percentile internals for survival models with integrated metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
  skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9013")
  skip_if_not_installed("yardstick", minimum_version = "1.3.0")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr, v = 2)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = c(0.01, 0.1))

  gctrl <- control_grid(save_pred = TRUE)

  # Grid search with integrated metrics ----------------------------------------

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

  set.seed(1)
  integrated_int <- int_pctl(grid_integrated_res, times = 1001)

  exp_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .lower = numeric(0),
      .estimate = numeric(0),
      .upper = numeric(0),
      .config = character(0),
      penalty = numeric(0)
    )

  expect_ptype(integrated_int, exp_ptype)
  expect_true(nrow(integrated_int) == nrow(grid))
  expect_equal(sort(integrated_int$penalty), grid$penalty)
  expect_true(all(integrated_int$.metric == "brier_survival_integrated"))
})


test_that("percentile internals for survival models with dynamic metrics", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
  skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9013")
  skip_if_not_installed("yardstick", minimum_version = "1.3.0")
  skip_if_not_installed("finetune", minimum_version = "1.1.0.9005")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- bootstraps(sim_tr, times = 5)

  time_points <- 10

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid <- tibble(cost_complexity = 10^c(-10, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with dynamic metrics ------------------------------------------------

  dyn_mtrc <- metric_set(brier_survival)

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

  set.seed(1)
  dyn_int <- int_pctl(aov_dyn_res, times = 1001)

  winners <- show_best(aov_dyn_res, eval_time = 10, metric = "brier_survival")

  exp_ptype <-
    tibble::tibble(
      cost_complexity = numeric(0),
      .metric = character(0),
      .estimator = character(0),
      .lower = numeric(0),
      .estimate = numeric(0),
      .upper = numeric(0),
      .config = character(0),
      .eval_time = numeric(0)
    )

  expect_ptype(dyn_int, exp_ptype)
  expect_true(nrow(dyn_int) == nrow(winners))
  expect_equal(sort(dyn_int$cost_complexity), sort(winners$cost_complexity))
  expect_true(all(dyn_int$.metric == "brier_survival"))
})


test_that("percentile internals for survival models mixture of metric types", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
  skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9013")
  skip_if_not_installed("yardstick", minimum_version = "1.3.0")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr, v = 2)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    bag_tree() %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with a mixture of metrics --------------------------------

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival
  )

  set.seed(2193)
  rs_mixed_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rsctrl
    )

  set.seed(1)
  mixed_int <- int_pctl(rs_mixed_res, times = 1001)

  exp_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .lower = numeric(0),
      .estimate = numeric(0),
      .upper = numeric(0),
      .config = character(0)
    )

  expect_ptype(mixed_int, exp_ptype)
  expect_true(nrow(mixed_int) == (length(time_points) + 2))
  expect_true(sum(mixed_int$.metric == "brier_survival") == length(time_points))
  expect_true(sum(mixed_int$.metric == "brier_survival_integrated") == 1)
  expect_true(sum(mixed_int$.metric == "concordance_survival") == 1)
  expect_true(all(
    !is.na(mixed_int$.eval_time[mixed_int$.metric == "brier_survival"])
  ))
  expect_true(all(is.na(mixed_int$.eval_time[
    mixed_int$.metric == "brier_survival_integrated"
  ])))
  expect_true(all(is.na(mixed_int$.eval_time[
    mixed_int$.metric == "concordance_survival"
  ])))
})

test_that("percentile internals for subset of eval times", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
  skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9013")
  skip_if_not_installed("yardstick", minimum_version = "1.3.0")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- vfold_cv(sim_tr, v = 2)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    bag_tree() %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with a mixture of metrics --------------------------------

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival
  )

  set.seed(2193)
  rs_mixed_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rsctrl
    )

  set.seed(1)
  mixed_int <- int_pctl(rs_mixed_res, times = 1001, eval_time = c(10, 5))

  exp_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .lower = numeric(0),
      .estimate = numeric(0),
      .upper = numeric(0),
      .config = character(0),
      .eval_time = numeric(0)
    )

  expect_ptype(mixed_int, exp_ptype)
  expect_true(nrow(mixed_int) == (2 + 2))
  expect_true(sum(mixed_int$.metric == "brier_survival") == 2)
  expect_true(sum(mixed_int$.metric == "brier_survival_integrated") == 1)
  expect_true(sum(mixed_int$.metric == "concordance_survival") == 1)
  expect_true(all(
    !is.na(mixed_int$.eval_time[mixed_int$.metric == "brier_survival"])
  ))
  expect_true(all(is.na(mixed_int$.eval_time[
    mixed_int$.metric == "brier_survival_integrated"
  ])))
  expect_true(all(is.na(mixed_int$.eval_time[
    mixed_int$.metric == "concordance_survival"
  ])))
})
