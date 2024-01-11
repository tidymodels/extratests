suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9011")
skip_if_not_installed("finetune", minimum_version = "1.1.0.9005")


test_that("fit_resamples save eval_time", {
  skip_if_not_installed("prodlim")

  # ------------------------------------------------------------------------------

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

  ## ------------------------------------------------------------------------------

  tree_spec <-
    decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit")

  ## ------------------------------------------------------------------------------

  srv_mtrc  <- metric_set(brier_survival)

  set.seed(2193)
  tree_res <-
    tree_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      sim_rs,
      metrics = srv_mtrc,
      eval_time = time_points
    )

  ## ------------------------------------------------------------------------------

  expect_true("eval_time" %in% names(attributes(tree_res)))
  expect_equal(attributes(tree_res)$eval_time, time_points)
  expect_equal(.get_tune_eval_times(tree_res), time_points)

})



test_that("tune*_() saves eval_time", {
  skip_if_not_installed("prodlim")

  tidymodels_prefer()

  ## ------------------------------------------------------------------------------

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

  ## ------------------------------------------------------------------------------

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid <- tibble(cost_complexity = 10^c(-10, -2, -1))

  srv_mtrc  <- metric_set(brier_survival)

  ## ------------------------------------------------------------------------------

  set.seed(2193)
  grid_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      sim_rs,
      grid = grid,
      metrics = srv_mtrc,
      eval_time = time_points
    )

  expect_true("eval_time" %in% names(attributes(grid_res)))
  expect_equal(attributes(grid_res)$eval_time, time_points)
  expect_equal(.get_tune_eval_times(grid_res), time_points)

  # ------------------------------------------------------------------------------

  set.seed(2193)
  bayes_res <-
    mod_spec %>%
    tune_bayes(
      event_time ~ X1 + X2,
      sim_rs,
      initial = grid_res,
      iter = 2,
      metrics = srv_mtrc,
      eval_time = time_points
    )

  expect_true("eval_time" %in% names(attributes(bayes_res)))
  expect_equal(attributes(bayes_res)$eval_time, time_points)
  expect_equal(.get_tune_eval_times(bayes_res), time_points)

  # ------------------------------------------------------------------------------

  set.seed(2193)
  sa_res <-
    mod_spec %>%
    tune_sim_anneal(
      event_time ~ X1 + X2,
      sim_rs,
      initial = grid_res,
      iter = 2,
      metrics = srv_mtrc,
      eval_time = time_points,
      control = control_sim_anneal(verbose_iter = FALSE)
    )

  expect_true("eval_time" %in% names(attributes(sa_res)))
  expect_equal(attributes(sa_res)$eval_time, time_points)
  expect_equal(.get_tune_eval_times(sa_res), time_points)

  ## ------------------------------------------------------------------------------

  set.seed(2193)
  anova_res <-
    mod_spec %>%
    tune_race_anova(
      event_time ~ X1 + X2,
      sim_rs,
      grid = grid,
      metrics = srv_mtrc,
      eval_time = time_points
    )

  expect_true("eval_time" %in% names(attributes(anova_res)))
  expect_equal(attributes(anova_res)$eval_time, time_points)
  expect_equal(.get_tune_eval_times(anova_res), time_points)

  ## ------------------------------------------------------------------------------

  set.seed(2193)
  wl_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      sim_rs,
      grid = grid,
      metrics = srv_mtrc,
      eval_time = time_points
    )

  expect_true("eval_time" %in% names(attributes(wl_res)))
  expect_equal(attributes(wl_res)$eval_time, time_points)
  expect_equal(.get_tune_eval_times(wl_res), time_points)

})


test_that("last_fit saves eval_time", {
  skip_if_not_installed("prodlim")

  tidymodels_prefer()

  # ------------------------------------------------------------------------------

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

  ## ------------------------------------------------------------------------------

  tree_spec <-
    decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit")

  ## ------------------------------------------------------------------------------

  srv_mtrc  <- metric_set(brier_survival)

  set.seed(2193)
  tree_res <-
    tree_spec %>%
    last_fit(
      event_time ~ X1 + X2,
      split,
      metrics = srv_mtrc,
      eval_time = time_points
    )

  ## ------------------------------------------------------------------------------

  expect_true("eval_time" %in% names(attributes(tree_res)))
  expect_equal(attributes(tree_res)$eval_time, time_points)
  expect_equal(.get_tune_eval_times(tree_res), time_points)

})

