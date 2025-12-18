suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9012")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")

test_that("last fit for survival models with static metric", {
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

  # test structure of results --------------------------------------------------

  expect_named(
    rs_static_res,
    c("splits", "id", ".metrics", ".notes", ".predictions", ".workflow"),
    ignore.order = TRUE
  )
  expect_false(".eval_time" %in% names(rs_static_res$.metrics[[1]]))
  expect_named(
    rs_static_res$.predictions[[1]],
    c(".pred_time", ".row", "event_time", ".config"),
    ignore.order = TRUE
  )
  expect_s3_class(rs_static_res$.workflow[[1]], "workflow")

  # test metric collection -----------------------------------------------------

  metric_sum <- collect_metrics(rs_static_res)
  exp_metric_sum <-
    tibble(
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_identical(nrow(metric_sum), 1L)
  expect_ptype(metric_sum, exp_metric_sum)
  expect_all_equal(metric_sum$.metric, "concordance_survival")

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
  expect_equal(nrow(unsum_pred), nrow(sim_te))

  sum_pred <- collect_predictions(rs_static_res, summarize = TRUE)
  expect_ptype(sum_pred, static_ptype[, names(static_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_te))
})

test_that("last fit for survival models with integrated metric", {
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

  # last fit for models with integrated metrics --------------------------------

  sint_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  rs_integrated_res <-
    survival_reg() %>%
    last_fit(
      event_time ~ X1 + X2,
      split = split,
      metrics = sint_mtrc,
      eval_time = time_points
    )

  # test structure of results --------------------------------------------------

  expect_named(
    rs_integrated_res,
    c("splits", "id", ".metrics", ".notes", ".predictions", ".workflow"),
    ignore.order = TRUE
  )
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
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_identical(nrow(metric_sum), 1L)
  expect_ptype(metric_sum, exp_metric_sum)
  expect_all_equal(metric_sum$.metric, "brier_survival_integrated")

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
  expect_equal(nrow(unsum_pred), nrow(sim_te))

  expect_ptype(unsum_pred$.pred[[1]], integrated_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(rs_integrated_res, summarize = TRUE)
  expect_ptype(sum_pred, integrated_ptype[, names(integrated_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_te))

  expect_ptype(sum_pred$.pred[[1]], integrated_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))
})

test_that("last fit for survival models with dynamic metric", {
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

  # last fit for models with dynamic metrics -----------------------------------

  dyn_mtrc <- metric_set(brier_survival)

  set.seed(2193)
  rs_dynamic_res <-
    survival_reg() %>%
    last_fit(
      event_time ~ X1 + X2,
      split = split,
      metrics = dyn_mtrc,
      eval_time = time_points
    )

  # test structure of results --------------------------------------------------

  expect_named(
    rs_dynamic_res,
    c("splits", "id", ".metrics", ".notes", ".predictions", ".workflow"),
    ignore.order = TRUE
  )
  expect_in(".eval_time", names(rs_dynamic_res$.metrics[[1]]))
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
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_identical(nrow(metric_sum), length(time_points))
  expect_ptype(metric_sum, exp_metric_sum)
  expect_all_equal(metric_sum$.metric, "brier_survival")

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
  expect_equal(nrow(unsum_pred), nrow(sim_te))

  expect_ptype(unsum_pred$.pred[[1]], dynamic_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(rs_dynamic_res, summarize = TRUE)
  expect_ptype(sum_pred, dynamic_ptype[, names(dynamic_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_te))

  expect_ptype(sum_pred$.pred[[1]], dynamic_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))
})

test_that("last fit for survival models with mixture of metrics", {
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

  # last fit for models with a mixture of metrics ------------------------------

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival
  )

  set.seed(2193)
  rs_mixed_res <-
    survival_reg() %>%
    last_fit(
      event_time ~ X1 + X2,
      split = split,
      metrics = mix_mtrc,
      eval_time = time_points
    )

  # test structure of results --------------------------------------------------

  expect_named(
    rs_mixed_res,
    c("splits", "id", ".metrics", ".notes", ".predictions", ".workflow"),
    ignore.order = TRUE
  )
  expect_in(".eval_time", names(rs_mixed_res$.metrics[[1]]))
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
      .estimate = numeric(0),
      .config = character(0)
    )

  expect_identical(nrow(metric_sum), length(time_points) + 2L)
  expect_ptype(metric_sum, exp_metric_sum)
  expect_identical(sum(is.na(metric_sum$.eval_time)), 2L)
  expect_equal(
    as.vector(table(metric_sum$.metric)),
    c(length(time_points), 1L, 1L)
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
  expect_equal(nrow(unsum_pred), nrow(sim_te))

  expect_ptype(unsum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(rs_mixed_res, summarize = TRUE)
  expect_ptype(sum_pred, mixed_ptype[, names(mixed_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_te))

  expect_ptype(sum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))
})
