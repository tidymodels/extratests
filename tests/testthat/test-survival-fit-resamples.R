suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(baguette))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9020")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("resampling survival models with static metric", {
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
    bag_tree() %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with static metrics --------------------------------------

  stc_mtrc <- metric_set(concordance_survival)

  set.seed(2193)
  rs_static_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = stc_mtrc,
      control = rsctrl
    )

  # passing in eval time when not required -------------------------------------

  expect_snapshot({
    set.seed(2193)
    rs_static_res <-
      mod_spec %>%
      fit_resamples(
        event_time ~ X1 + X2,
        resamples = sim_rs,
        metrics = stc_mtrc,
        control = rsctrl,
        eval_time = time_points
      )
  })

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(rs_static_res$.metrics[[1]]))
  expect_named(
    rs_static_res$.predictions[[1]],
    c(".pred_time", ".row", "event_time", ".config"),
    ignore.order = TRUE
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(rs_static_res)
  exp_metric_sum <-
    tibble(
      .metric = character(0),
      .estimator = character(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_sum) == 1)
  expect_ptype(metric_sum, exp_metric_sum)
  expect_true(all(metric_sum$.metric == "concordance_survival"))

  metric_all <- collect_metrics(rs_static_res, summarize = FALSE)
  exp_metric_all <-
    tibble(
      id = character(0),
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_all) == 10)
  expect_ptype(metric_all, exp_metric_all)

  expect_true(all(metric_all$.metric == "concordance_survival"))

  # test prediction collection -------------------------------------------------

  static_ptype <- tibble::tibble(
    .pred_time = numeric(0),
    id = character(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  unsum_pred <- collect_predictions(rs_static_res)
  expect_ptype(unsum_pred, static_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr))

  sum_pred <- collect_predictions(rs_static_res, summarize = TRUE)
  expect_ptype(sum_pred, static_ptype[, names(static_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr))
})

test_that("resampling survival models with integrated metric", {
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
    bag_tree() %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with integrated metrics ----------------------------------

  sint_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  rs_integrated_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = rsctrl
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(rs_integrated_res$.metrics[[1]]))
  expect_named(
    rs_integrated_res$.predictions[[1]],
    c(".pred", ".row", "event_time", ".config"),
    ignore.order = TRUE
  )

  expect_true(is.list(rs_integrated_res$.predictions[[1]]$.pred))
  expect_named(
    rs_integrated_res$.predictions[[1]]$.pred[[1]],
    c(".eval_time", ".pred_survival", ".weight_censored"),
    ignore.order = TRUE
  )
  expect_equal(
    rs_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(rs_integrated_res)
  exp_metric_sum <-
    tibble(
      .metric = character(0),
      .estimator = character(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_sum) == 1)
  expect_ptype(metric_sum, exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival_integrated"))

  metric_all <- collect_metrics(rs_integrated_res, summarize = FALSE)
  exp_metric_all <-
    tibble(
      id = character(0),
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_all) == 10)
  expect_ptype(metric_all, exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival_integrated"))

  # test prediction collection -------------------------------------------------

  integrated_ptype <- tibble::tibble(
    .pred = list(),
    id = character(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  integrated_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(rs_integrated_res)
  expect_ptype(unsum_pred, integrated_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr))

  expect_ptype(unsum_pred$.pred[[1]], integrated_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(rs_integrated_res, summarize = TRUE)
  expect_ptype(sum_pred, integrated_ptype[, names(integrated_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr))

  expect_ptype(sum_pred$.pred[[1]], integrated_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))
})

test_that("resampling survival models with dynamic metric", {
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
    bag_tree() %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with dynamic metrics -------------------------------------

  dyn_mtrc <- metric_set(brier_survival)

  set.seed(2193)
  rs_dynamic_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = dyn_mtrc,
      eval_time = time_points,
      control = rsctrl
    )

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(rs_dynamic_res$.metrics[[1]]))

  expect_named(
    rs_dynamic_res$.predictions[[1]],
    c(".pred", ".row", "event_time", ".config"),
    ignore.order = TRUE
  )
  expect_true(is.list(rs_dynamic_res$.predictions[[1]]$.pred))

  expect_named(
    rs_dynamic_res$.predictions[[1]]$.pred[[1]],
    c(".eval_time", ".pred_survival", ".weight_censored"),
    ignore.order = TRUE
  )
  expect_equal(
    rs_dynamic_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(rs_dynamic_res)
  exp_metric_sum <-
    tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_sum) == length(time_points))
  expect_ptype(metric_sum, exp_metric_sum)
  expect_true(all(metric_sum$.metric == "brier_survival"))

  metric_all <- collect_metrics(rs_dynamic_res, summarize = FALSE)
  exp_metric_all <-
    tibble(
      id = character(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_all) == length(time_points) * nrow(sim_rs))
  expect_ptype(metric_all, exp_metric_all)
  expect_true(all(metric_all$.metric == "brier_survival"))

  # test prediction collection -------------------------------------------------

  dynamic_ptype <- tibble::tibble(
    .pred = list(),
    id = character(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  dynamic_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(rs_dynamic_res)
  expect_ptype(unsum_pred, dynamic_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr))

  expect_ptype(unsum_pred$.pred[[1]], dynamic_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(rs_dynamic_res, summarize = TRUE)
  expect_ptype(sum_pred, dynamic_ptype[, names(dynamic_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr))

  expect_ptype(sum_pred$.pred[[1]], dynamic_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))
})

test_that("resampling survival models with linear_pred metric", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("yardstick", minimum_version = "1.3.2.9000")
  skip_if_not_installed("tune", minimum_version = "2.0.1.9001")

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

  mod_spec <-
    proportional_hazards(penalty = 0.01) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with linear_pred metrics --------------------------------

  linpred_mtrc <- metric_set(royston_survival)

  set.seed(2193)
  rs_linpred_res <-
    mod_spec %>%
    fit_resamples(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      metrics = linpred_mtrc,
      control = rsctrl
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(rs_linpred_res$.metrics[[1]]))
  expect_named(
    rs_linpred_res$.predictions[[1]],
    c(".pred_linear_pred", ".row", "event_time", ".config"),
    ignore.order = TRUE
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(rs_linpred_res)
  exp_metric_sum <-
    tibble(
      .metric = character(0),
      .estimator = character(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_sum) == 1)
  expect_ptype(metric_sum, exp_metric_sum)
  expect_true(all(metric_sum$.metric == "royston_survival"))

  metric_all <- collect_metrics(rs_linpred_res, summarize = FALSE)
  exp_metric_all <-
    tibble(
      id = character(0),
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_all) == 10)
  expect_ptype(metric_all, exp_metric_all)
  expect_true(all(metric_all$.metric == "royston_survival"))

  # test prediction collection -------------------------------------------------

  linpred_ptype <- tibble::tibble(
    .pred_linear_pred = numeric(0),
    id = character(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  unsum_pred <- collect_predictions(rs_linpred_res)
  expect_ptype(unsum_pred, linpred_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr))

  sum_pred <- collect_predictions(rs_linpred_res, summarize = TRUE)
  expect_ptype(sum_pred, linpred_ptype[, names(linpred_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr))
})

test_that("resampling survival models mixture of metric types", {
  skip_if_not_installed("prodlim")
  skip_if_not_installed("tune", minimum_version = "1.3.0.9005")

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

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(rs_mixed_res$.metrics[[1]]))
  expect_named(
    rs_mixed_res$.predictions[[1]],
    c(".pred", ".row", ".pred_time", "event_time", ".config"),
    ignore.order = TRUE
  )
  expect_true(is.list(rs_mixed_res$.predictions[[1]]$.pred))
  expect_named(
    rs_mixed_res$.predictions[[1]]$.pred[[1]],
    c(".eval_time", ".pred_survival", ".weight_censored"),
    ignore.order = TRUE
  )
  expect_equal(
    rs_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(rs_mixed_res)
  exp_metric_sum <-
    tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_sum) == length(time_points) + 2)
  expect_ptype(metric_sum, exp_metric_sum)
  expect_true(sum(is.na(metric_sum$.eval_time)) == 2)
  expect_equal(
    as.vector(table(metric_sum$.metric)),
    c(length(time_points), 1L, 1L)
  )

  metric_all <- collect_metrics(rs_mixed_res, summarize = FALSE)
  exp_metric_all <-
    tibble(
      id = character(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_all) == (length(time_points) + 2) * nrow(sim_rs))
  expect_ptype(metric_all, exp_metric_all)
  expect_true(sum(is.na(metric_all$.eval_time)) == 2 * nrow(sim_rs))
  expect_equal(
    as.vector(table(metric_all$.metric)),
    c(length(time_points), 1L, 1L) * nrow(sim_rs)
  )

  # test prediction collection -------------------------------------------------

  mixed_ptype <- tibble::tibble(
    .pred = list(),
    .pred_time = numeric(0),
    id = character(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  mixed_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(rs_mixed_res)
  expect_ptype(unsum_pred, mixed_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr))

  expect_ptype(unsum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(rs_mixed_res, summarize = TRUE)
  expect_ptype(sum_pred, mixed_ptype[, names(mixed_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr))

  expect_ptype(sum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test show_best() -----------------------------------------------------------

  expect_snapshot(show_best(rs_mixed_res, metric = "brier_survival"))
  expect_snapshot(show_best(
    rs_mixed_res,
    metric = "brier_survival",
    eval_time = 1
  ))
  expect_snapshot(
    show_best(rs_mixed_res, metric = "brier_survival", eval_time = c(1.001)),
    error = TRUE
  )
  expect_snapshot(
    show_best(rs_mixed_res, metric = "brier_survival", eval_time = c(1, 3))
  )
  expect_snapshot(
    show_best(rs_mixed_res, metric = "brier_survival_integrated")
  )
})

test_that("resampling survival models mixture of metric types including linear_pred", {
  # this is a separate to above because the other one uses a bagged tree model
  # which does not give us the linear predictor
  skip_if_not_installed("prodlim")
  skip_if_not_installed("yardstick", minimum_version = "1.3.2.9000")
  skip_if_not_installed("tune", minimum_version = "2.0.1.9001")

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
    proportional_hazards(penalty = 0.01) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  rsctrl <- control_resamples(save_pred = TRUE)

  # resampling models with a mixture of metrics including linear_pred ----------

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival,
    royston_survival
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

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(rs_mixed_res$.metrics[[1]]))
  expect_named(
    rs_mixed_res$.predictions[[1]],
    c(
      ".pred",
      ".row",
      ".pred_time",
      ".pred_linear_pred",
      "event_time",
      ".config"
    ),
    ignore.order = TRUE
  )
  expect_true(is.list(rs_mixed_res$.predictions[[1]]$.pred))
  expect_named(
    rs_mixed_res$.predictions[[1]]$.pred[[1]],
    c(".eval_time", ".pred_survival", ".weight_censored"),
    ignore.order = TRUE
  )
  expect_equal(
    rs_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(rs_mixed_res)
  exp_metric_sum <-
    tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      mean = numeric(0),
      n = integer(0),
      std_err = numeric(0),
      .config = character(0)
    )

  # 4 time points for brier_survival + 1 for integrated + 1 for concordance + 1 for royston
  expect_true(nrow(metric_sum) == length(time_points) + 3)
  expect_ptype(metric_sum, exp_metric_sum)
  # concordance, integrated, and royston don't have eval_time
  expect_true(sum(is.na(metric_sum$.eval_time)) == 3)
  expect_equal(
    as.vector(table(metric_sum$.metric)),
    c(length(time_points), 1L, 1L, 1L)
  )

  metric_all <- collect_metrics(rs_mixed_res, summarize = FALSE)
  exp_metric_all <-
    tibble(
      id = character(0),
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_true(nrow(metric_all) == (length(time_points) + 3) * nrow(sim_rs))
  expect_ptype(metric_all, exp_metric_all)
  expect_true(sum(is.na(metric_all$.eval_time)) == 3 * nrow(sim_rs))
  expect_equal(
    as.vector(table(metric_all$.metric)),
    c(length(time_points), 1L, 1L, 1L) * nrow(sim_rs)
  )

  # test prediction collection -------------------------------------------------

  mixed_ptype <- tibble::tibble(
    .pred = list(),
    .pred_time = numeric(0),
    .pred_linear_pred = numeric(0),
    id = character(0),
    .row = integer(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  mixed_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  unsum_pred <- collect_predictions(rs_mixed_res)
  expect_ptype(unsum_pred, mixed_ptype)
  expect_equal(nrow(unsum_pred), nrow(sim_tr))

  expect_ptype(unsum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(rs_mixed_res, summarize = TRUE)
  expect_ptype(sum_pred, mixed_ptype[, names(mixed_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr))

  expect_ptype(sum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  expect_no_error(show_best(rs_mixed_res, metric = "royston_survival"))
})
