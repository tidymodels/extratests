suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9012")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9003")

test_that("grid tuning survival models with importance case weights", {
  skip_if_not_installed("prodlim")

  ## ------------------------------------------------------------------------------

  time_points <- c(10, 1, 5, 15)

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet", path_values = grid$penalty) %>%
    set_mode("censored regression")

  gctrl <- control_grid(save_pred = TRUE, extract = function(x) {
    coef(extract_fit_engine(x))
  })

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival
  )

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2) %>%
    as_tibble()

  ## unweighted ----------------------------------------------------------------

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_rs <- vfold_cv(sim_tr)

  set.seed(2193)
  weights_none_tune_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  ## importance weighted -------------------------------------------------------

  set.seed(3)
  wts <- c(rep(0.001, 50), runif(nrow(sim_dat) - 50))

  imp_wts <- sim_dat %>%
    mutate(imp_wts = importance_weights(wts))

  set.seed(2)
  imp_split <- initial_split(imp_wts)
  imp_tr <- training(imp_split)
  imp_rs <- vfold_cv(imp_tr)

  imp_wflow <-
    workflow() %>%
    add_model(mod_spec) %>%
    add_formula(event_time ~ X1 + X2) %>%
    add_case_weights(imp_wts)

  set.seed(2193)
  weights_imp_tune_res <-
    imp_wflow %>%
    tune_grid(
      resamples = imp_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  expect_false(
    identical(
      weights_none_tune_res$.extracts[[1]]$.extracts[[1]],
      weights_imp_tune_res$.extracts[[1]]$.extracts[[1]]
    )
  )

  expect_false(
    identical(
      collect_metrics(weights_none_tune_res),
      collect_metrics(weights_imp_tune_res)
    )
  )
})


test_that("grid tuning survival models with frequency case weights", {
  skip_if_not_installed("prodlim")

  ## ------------------------------------------------------------------------------

  time_points <- c(10, 1, 5, 15)

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  mod_spec <-
    proportional_hazards(penalty = 0.01, mixture = 1) %>%
    set_engine("glmnet", path_values = grid$penalty) %>%
    set_mode("censored regression")

  rctrl <- control_resamples(extract = function(x) extract_fit_engine(x))

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival
  )

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2) %>%
    as_tibble()

  ## unweighted ----------------------------------------------------------------

  sim_rs <- apparent(sim_dat %>% dplyr::slice(-(1:50)))

  set.seed(2193)
  weights_none_tune_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  ## freq weighted -------------------------------------------------------------

  set.seed(3)
  wts <- c(rep(0, 50), rep(1L, nrow(sim_dat) - 50))

  frq_wts <- sim_dat %>%
    mutate(frq_wts = frequency_weights(wts))

  frq_rs <- apparent(frq_wts)

  frq_wflow <-
    workflow() %>%
    add_model(mod_spec) %>%
    add_formula(event_time ~ X1 + X2) %>%
    add_case_weights(frq_wts)

  set.seed(2193)
  weights_frq_tune_res <-
    frq_wflow %>%
    fit_resamples(
      resamples = frq_rs,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  expect_identical(
    coef(weights_none_tune_res$.extracts[[1]]$.extracts[[1]]),
    coef(weights_frq_tune_res$.extracts[[1]]$.extracts[[1]])
  )
})
