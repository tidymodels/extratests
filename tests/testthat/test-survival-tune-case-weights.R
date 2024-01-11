suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9010")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("grid tuning survival models with case weights", {
  skip_if_not_installed("prodlim")

  ## ------------------------------------------------------------------------------

  time_points <- c(10, 1, 5, 15)

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet", path_values = grid$penalty) %>%
    set_mode("censored regression")

  gctrl <- control_grid(save_pred = TRUE, extract = function(x) coef(extract_fit_engine(x)))

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

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
    isTRUE(
      all.equal(
        weights_none_tune_res$.extracts[[1]]$.extracts[[1]],
        weights_imp_tune_res$.extracts[[1]]$.extracts[[1]]
      )
    )
  )

  expect_false(
    isTRUE(
      all.equal(
        collect_metrics(weights_none_tune_res),
        collect_metrics(weights_imp_tune_res)
      )
    )
  )


  ## freq weighted -------------------------------------------------------------

  # → A | error:   Failed to compute `brier_survival()`.
  #                Caused by error:
  #                ℹ In argument: `.estimate = brier_survival_impl(...)`.
  #                ℹ In group 1: `.eval_time = 1`.
  #                Caused by error in `vec_math()`:
  #                ! `vec_math.hardhat_frequency_weights()` not implemented.

  # set.seed(3)
  # wts <- c(rep(0L, 50), rep(1L, nrow(sim_dat) - 50))
  #
  # frq_wts <- sim_dat %>%
  #   mutate(frq_wts = frequency_weights(wts))
  #
  # set.seed(2)
  # frq_split <- initial_split(frq_wts)
  # frq_tr <- training(frq_split)
  # frq_rs <- vfold_cv(frq_tr)
  #
  # frq_wflow <-
  #   workflow() %>%
  #   add_model(mod_spec) %>%
  #   add_formula(event_time ~ X1 + X2) %>%
  #   add_case_weights(frq_wts)
  #
  # set.seed(2193)
  # weights_frq_tune_res <-
  #   frq_wflow %>%
  #   tune_grid(
  #     resamples = frq_rs,
  #     grid = grid,
  #     metrics = mix_mtrc,
  #     eval_time = time_points,
  #     control = gctrl
  #   )
  #
  # expect_false(
  #   isTRUE(
  #     all.equal(
  #       weights_none_tune_res$.extracts[[1]]$.extracts[[1]],
  #       weights_frq_tune_res$.extracts[[1]]$.extracts[[1]]
  #     )
  #   )
  # )
  #
  # expect_false(
  #   isTRUE(
  #     all.equal(
  #       collect_metrics(weights_none_tune_res),
  #       collect_metrics(weights_frq_tune_res)
  #     )
  #   )
  # )

})
