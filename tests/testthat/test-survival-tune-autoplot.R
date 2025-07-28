suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9020")
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
  stc_grid_static_res <-
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
      initial = stc_grid_static_res
    )

  # ------------------------------------------------------------------------------

  stc_marginal <- autoplot(stc_grid_static_res)

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  expect_equal(exp_data_ptype, stc_marginal$data[0,])
  expect_equal(
    rlang::expr_text(stc_marginal$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(stc_marginal$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(stc_marginal))

  # ------------------------------------------------------------------------------

  stc_perf <- autoplot(sa_static_res, type = "performance")

  exp_data_ptype <-
    tibble::tibble(
      trees = numeric(0),
      .metric = character(0),
      .estimator = character(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0),
      .iter = integer(0)
    )
  expect_equal(exp_data_ptype, stc_perf$data[0,])
  expect_equal(
    rlang::expr_text(stc_perf$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(stc_perf$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(stc_perf))

  # ------------------------------------------------------------------------------

  stc_param <- autoplot(sa_static_res, type = "parameters")

  exp_data_ptype <-
    tibble::tibble(
      .iter = integer(0),
      name = character(0),
      value = double(0)
    )
  expect_equal(exp_data_ptype, stc_param$data[0,])
  expect_equal(
    rlang::expr_text(stc_param$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(stc_param$mapping$y),
    "~value"
  )
  expect_snapshot(ggplot2::get_labs(stc_param))


})


test_that("autoplot-ting survival models with integrated metric", {

  skip_if_not_installed("glmnet")
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
  sim_rs <- bootstraps(sim_tr, times = 20)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = tune(), engine = "glmnet") %>%
    set_mode("censored regression")

  grid <-
    mod_spec %>%
    extract_parameter_set_dials() %>%
    grid_regular(levels = 3)

  gctrl <- control_grid(save_pred = TRUE)

  int_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  int_grid_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = int_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  set.seed(2193)
  int_bayes_res <-
    mod_spec %>%
    tune_bayes(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      initial = int_grid_res,
      iter = 5,
      metrics = int_mtrc,
      eval_time = time_points
    )

  # ------------------------------------------------------------------------------

  int_grid <- autoplot(int_grid_res)

  exp_data_ptype <-
    tibble::tibble(
     `Proportion of Lasso Penalty` = character(0),
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, int_grid$data[0,])
  expect_equal(
    rlang::expr_text(int_grid$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(int_grid$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(int_grid$mapping$colour),
    "~`Proportion of Lasso Penalty`"
  )
  expect_equal(
    rlang::expr_text(int_grid$mapping$group),
    "~`Proportion of Lasso Penalty`"
  )
  expect_snapshot(ggplot2::get_labs(int_grid))

  # ------------------------------------------------------------------------------

  int_marginal <- autoplot(int_bayes_res)

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, int_marginal$data[0,])
  expect_equal(
    rlang::expr_text(int_marginal$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(int_marginal$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(int_marginal$facet$params$facets$name),
    "~name"
  )
  expect_snapshot(ggplot2::get_labs(int_marginal))

  # ------------------------------------------------------------------------------

  int_perf <- autoplot(int_bayes_res, type = "performance")

  exp_data_ptype <-
    tibble::tibble(
      penalty = numeric(0),
      mixture = numeric(0),
      .metric = character(0),
      .estimator = character(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0),
      .iter = integer(0)
    )
  expect_equal(exp_data_ptype, int_perf$data[0,])
  expect_equal(
    rlang::expr_text(int_perf$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(int_perf$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(int_perf))

  # ------------------------------------------------------------------------------

  int_param <- autoplot(int_bayes_res, type = "parameters")

  exp_data_ptype <-
    tibble::tibble(
      .iter = integer(0),
      name = character(0),
      value = double(0)
    )
  expect_equal(exp_data_ptype, int_param$data[0,])
  expect_equal(
    rlang::expr_text(int_param$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(int_param$mapping$y),
    "~value"
  )
  expect_equal(
    rlang::expr_text(int_param$facet$params$facets$name),
    "~name"
  )
  expect_snapshot(ggplot2::get_labs(int_param))


})

test_that("autoplot-ting survival models with dynamic metric", {

  skip_if_not_installed("rpart")
  skip_if_not_installed("prodlim")

  # ------------------------------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(1000) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_validation_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- validation_set(split)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune(), min_n = tune()) %>%
    set_engine("rpart") %>%
    set_mode("censored regression")

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)

  # Bayes with dynamic metric --------------------------------------------------

  dyn_mtrc  <- metric_set(brier_survival)

  set.seed(2193)
  grid_dynamic_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = 10,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  expect_snapshot({
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
        initial = grid_dynamic_res
      )
  })

  # ------------------------------------------------------------------------------

  # default to first time
  dyn_grid <- autoplot(grid_dynamic_res)

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, dyn_grid$data[0,])
  expect_equal(
    unique(dyn_grid$data$name),
    c("Cost-Complexity Parameter (log-10)", "Minimal Node Size")
  )
  expect_equal(
    rlang::expr_text(dyn_grid$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(dyn_grid$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(dyn_grid$facet$params$facets$name),
    "~name"
  )
  expect_snapshot(ggplot2::get_labs(dyn_grid))

  # ------------------------------------------------------------------------------

  # multiple times
  dyn_mult_grid <- autoplot(grid_dynamic_res, eval_time = c(1, 10))

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, dyn_mult_grid$data[0,])
  expect_equal(
    unique(dyn_mult_grid$data$name),
    c("Cost-Complexity Parameter (log-10)", "Minimal Node Size")
  )
  expect_equal(
    unique(dyn_mult_grid$data$.metric),
    c("brier_survival @ 1", "brier_survival @10")
  )
  expect_equal(
    rlang::expr_text(dyn_mult_grid$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(dyn_mult_grid$mapping$y),
    "~mean"
  )
  expect_equal(
    names(dyn_mult_grid$facet$params$rows),
    ".metric"
  )
  expect_equal(
    names(dyn_mult_grid$facet$params$cols),
    "name"
  )
  expect_snapshot(ggplot2::get_labs(dyn_mult_grid))

  # ------------------------------------------------------------------------------

  dyn_marginal <- autoplot(bayes_dynamic_res)

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, dyn_marginal$data[0,])
  expect_equal(
    rlang::expr_text(dyn_marginal$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(dyn_marginal$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(dyn_marginal$facet$params$facets$name),
    "~name"
  )
  expect_snapshot(ggplot2::get_labs(dyn_marginal))

  # ------------------------------------------------------------------------------

  # multiple times
  dyn_mult_marginal <- autoplot(bayes_dynamic_res, eval_time = c(1, 5))

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, dyn_mult_marginal$data[0,])
  expect_equal(
    unique(dyn_mult_marginal$data$name),
    c("Cost-Complexity Parameter (log-10)", "Minimal Node Size")
  )
  expect_equal(
    unique(dyn_mult_marginal$data$.metric),
    c("brier_survival @1", "brier_survival @5")
  )

  expect_equal(
    rlang::expr_text(dyn_mult_marginal$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(dyn_mult_marginal$mapping$y),
    "~mean"
  )
  expect_equal(
    names(dyn_mult_marginal$facet$params$rows),
    ".metric"
  )
  expect_equal(
    names(dyn_mult_marginal$facet$params$cols),
    "name"
  )
  expect_snapshot(ggplot2::get_labs(dyn_mult_marginal))

  # ------------------------------------------------------------------------------

  dyn_perf <- autoplot(bayes_dynamic_res, type = "performance")

  exp_data_ptype <-
    tibble::tibble(
      cost_complexity = numeric(0),
      min_n = integer(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0),
      .iter = integer(0)
    )

  expect_equal(exp_data_ptype, dyn_perf$data[0,])
  expect_equal(
    rlang::expr_text(dyn_perf$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(dyn_perf$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(dyn_perf))

  # ------------------------------------------------------------------------------

  # multiple times
  dyn_mult_perf <- autoplot(bayes_dynamic_res, type = "performance", eval_time = c(1, 5))

  exp_data_ptype <-
    tibble::tibble(
      cost_complexity = numeric(0),
      min_n = integer(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0),
      .iter = integer(0)
    )

  expect_equal(exp_data_ptype, dyn_mult_perf$data[0,])
  expect_equal(
    unique(dyn_mult_perf$data$.metric),
    c("brier_survival @1", "brier_survival @5")
  )

  expect_equal(
    rlang::expr_text(dyn_mult_perf$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(dyn_mult_perf$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(dyn_marginal$facet$params$facets$name),
    "~name"
  )
  expect_snapshot(ggplot2::get_labs(dyn_marginal))

  # ------------------------------------------------------------------------------

  dyn_param <- autoplot(bayes_dynamic_res, type = "parameters")

  exp_data_ptype <-
    tibble::tibble(
      .iter = integer(0),
      name = character(0),
      value = double(0)
    )
  expect_equal(exp_data_ptype, dyn_param$data[0,])
  expect_equal(
    rlang::expr_text(dyn_param$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(dyn_param$mapping$y),
    "~value"
  )
  expect_equal(
    rlang::expr_text(dyn_param$facet$params$facets$name),
    "~name"
  )
  expect_snapshot(ggplot2::get_labs(dyn_param))

  # ------------------------------------------------------------------------------

  expect_snapshot(
    dyn_mult_param <- autoplot(bayes_dynamic_res, type = "parameters", eval_time = c(10, 15))
  )
  exp_data_ptype <-
    tibble::tibble(
      .iter = integer(0),
      name = character(0),
      value = double(0)
    )
  expect_equal(exp_data_ptype, dyn_mult_param$data[0,])

  expect_equal(
    rlang::expr_text(dyn_mult_param$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(dyn_mult_param$mapping$y),
    "~value"
  )
  expect_equal(
    rlang::expr_text(dyn_mult_param$facet$params$facets$name),
    "~name"
  )
  expect_snapshot(ggplot2::get_labs(dyn_mult_param))

})

test_that("autoplot-ting survival models with different metric types", {

  skip_if_not_installed("rpart")
  skip_if_not_installed("prodlim")

  # ------------------------------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(1000) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_validation_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- validation_set(split)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune(), min_n = tune()) %>%
    set_engine("rpart") %>%
    set_mode("censored regression")

  gctrl <- control_grid(save_pred = TRUE)
  bctrl <- control_bayes(save_pred = TRUE)

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
  grid_mixed_res <-
    mod_spec %>%
    tune_grid(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = mix_mtrc,
      eval_time = time_points,
      control = gctrl
    )

  expect_snapshot({
    set.seed(2193)
    bayes_mixed_res <-
      mod_spec %>%
      tune_bayes(
        event_time ~ X1 + X2,
        resamples = sim_rs,
        iter = 2,
        metrics = mix_mtrc,
        eval_time = time_points,
        initial = grid_mixed_res,
        control = bctrl
      )
  })

  # ------------------------------------------------------------------------------

  # default to first time
  mix_grid <- autoplot(grid_mixed_res)

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, mix_grid$data[0,])
  expect_equal(
    unique(mix_grid$data$name),
    c("Tree Depth")
  )
  expect_equal(
    rlang::expr_text(mix_grid$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(mix_grid$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(mix_grid$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_grid$data$.metric)),
    c("brier_survival @10", "brier_survival_integrated", "concordance_survival")
  )
  expect_snapshot(ggplot2::get_labs(mix_grid))

  # ------------------------------------------------------------------------------

  # multiple times
  mix_mult_grid <- autoplot(grid_mixed_res, eval_time = c(1, 10))

  exp_data_ptype <-
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )

  expect_equal(exp_data_ptype, mix_mult_grid$data[0,])
  expect_equal(
    unique(mix_mult_grid$data$name),
    c("Tree Depth")
  )
  expect_equal(
    rlang::expr_text(mix_mult_grid$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(mix_mult_grid$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(mix_mult_grid$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_mult_grid$data$.metric)),
    c("brier_survival @ 1", "brier_survival @10", "brier_survival_integrated",
      "concordance_survival")
  )
  expect_snapshot(ggplot2::get_labs(mix_mult_grid))

  # ------------------------------------------------------------------------------

  mix_marginal <- autoplot(bayes_mixed_res)

  expect_equal(exp_data_ptype, mix_marginal$data[0,])
  expect_equal(
    unique(mix_marginal$data$name),
    c("Tree Depth")
  )
  expect_equal(
    rlang::expr_text(mix_marginal$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(mix_marginal$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(mix_marginal$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_marginal$data$.metric)),
    c("brier_survival @10", "brier_survival_integrated",
      "concordance_survival")
  )
  expect_snapshot(ggplot2::get_labs(mix_marginal))

  # ------------------------------------------------------------------------------

  # multiple times
  mix_mult_marginal <- autoplot(bayes_mixed_res, eval_time = c(1, 5))

  expect_equal(exp_data_ptype, mix_mult_marginal$data[0,])
  expect_equal(
    unique(mix_mult_marginal$data$name),
    c("Tree Depth")
  )
  expect_equal(
    rlang::expr_text(mix_mult_marginal$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(mix_mult_marginal$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(mix_mult_marginal$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_mult_marginal$data$.metric)),
    c("brier_survival @ 1", "brier_survival @ 5", "brier_survival_integrated",
      "concordance_survival")
  )
  expect_snapshot(ggplot2::get_labs(mix_mult_marginal))

  # ------------------------------------------------------------------------------

  mix_perf <- autoplot(bayes_mixed_res, type = "performance")

  exp_data_ptype <-
    tibble::tibble(
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

  expect_equal(exp_data_ptype, mix_perf$data[0,])
  expect_equal(
    rlang::expr_text(mix_perf$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_perf$data$.metric)),
    c("brier_survival @10", "brier_survival_integrated", "concordance_survival")
  )
  expect_equal(
    rlang::expr_text(mix_perf$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(mix_perf$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(mix_perf))

  # ------------------------------------------------------------------------------

  # multiple times
  mix_mult_perf <- autoplot(bayes_mixed_res, type = "performance", eval_time = c(1, 5))


  exp_data_ptype <-
    tibble::tibble(
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

  expect_equal(exp_data_ptype, mix_mult_perf$data[0,])
  expect_equal(
    rlang::expr_text(mix_mult_perf$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_mult_perf$data$.metric)),
    c("brier_survival @ 1", "brier_survival @ 5",
      "brier_survival_integrated",
      "concordance_survival")
  )
  expect_equal(
    rlang::expr_text(mix_mult_perf$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(mix_mult_perf$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(mix_mult_perf))

  # ------------------------------------------------------------------------------

  mix_param <- autoplot(bayes_mixed_res, type = "parameters")

  exp_data_ptype <-
    tibble::tibble(
      .iter = integer(0),
      name = character(0),
      value = double(0)
    )
  expect_equal(exp_data_ptype, mix_param$data[0,])
  expect_equal(
    rlang::expr_text(mix_param$mapping$x),
    "~.iter"
  )
  expect_equal(
    rlang::expr_text(mix_param$mapping$y),
    "~value"
  )

  expect_snapshot(ggplot2::get_labs(mix_param))

  expect_snapshot(
    mix_mult_param <- autoplot(bayes_mixed_res, type = "parameters", eval_time = c(10, 15))
  )

})

test_that("autoplot() warning for unneeded evaluation times", {
  skip_if_not_installed("glmnet")

  lung_surv <- lung %>%
    dplyr::mutate(surv = Surv(time, status), .keep = "unused")
  set.seed(2193)
  tune_res <-
    proportional_hazards(penalty = tune(), engine = "glmnet") %>%
    tune_grid(
      surv ~ .,
      resamples = vfold_cv(lung_surv, 2),
      grid = 2,
      metrics = metric_set(concordance_survival, brier_survival_integrated),
      eval_time = c(10, 20)
    )

  expect_snapshot(
    p1 <- autoplot(tune_res, eval_time = 10, metric = "concordance_survival")
  )
  expect_snapshot(
    p2 <- autoplot(tune_res, eval_time = 10, metric = "brier_survival_integrated")
  )

})



