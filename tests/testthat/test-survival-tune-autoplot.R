suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9016")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
skip_if_not_installed("finetune", minimum_version = "1.1.0.9005")

test_that("autoplot-ting survival models with static metric", {
  skip_if_not_installed("mboost")
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

  # Simulated annealing with static metrics ------------------------------------

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

  init_marginal <- autoplot(init_grid_static_res)

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  expect_equal(exp_data_ptype, init_marginal$data[0,])
  expect_equal(
    rlang::expr_text(init_marginal$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(init_marginal$mapping$y),
    "~mean"
  )
  expect_equal(
    init_marginal$labels,
    list(x = c(trees = "# Trees"), y = "concordance_survival")
  )

})

