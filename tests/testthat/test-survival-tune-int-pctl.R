suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

test_that("percentile internals for survival models with static metric", {
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
  sim_rs <- vfold_cv(sim_tr)

  time_points <- c(10, 1, 5, 15)

  # last fit for models with static metrics ------------------------------------

  stc_mtrc  <- metric_set(concordance_survival)

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

  expect_equal(static_int[0,], exp_ptype)
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
      .config = character(0)
    )

  expect_equal(integrated_int[0,], exp_ptype)
  expect_true(nrow(integrated_int) == nrow(grid))
  expect_equal(sort(integrated_int$penalty), grid$penalty)
  expect_true(all(integrated_int$.metric == "brier_survival_integrated"))

})
