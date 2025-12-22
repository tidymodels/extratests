suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9020")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
skip_if_not_installed("finetune", minimum_version = "1.1.0.9006")

test_that("race tuning (win_loss) survival models with static metric", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

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
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid <- tibble(cost_complexity = 10^c(-10, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with static metrics -------------------------------------------------

  stc_mtrc <- metric_set(concordance_survival)

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

  num_final_wl <-
    unique(
      show_best(wl_static_res, metric = "concordance_survival")$cost_complexity
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(wl_static_res$.metrics[[1]]))

  expect_named(
    wl_static_res$.predictions[[1]],
    c(".pred_time", ".row", "cost_complexity", "event_time", ".config"),
    ignore.order = TRUE
  )

  # test race plot -------------------------------------------------------------

  stc_race_plot <- plot_race(wl_static_res)

  expect_ptype(
    stc_race_plot$data,
    tibble::tibble(
      .config = character(0),
      mean = numeric(0),
      n = integer(0),
      stage = integer(0)
    )
  )

  expect_equal(
    rlang::expr_text(stc_race_plot$mapping$x),
    "~stage"
  )
  expect_equal(
    rlang::expr_text(stc_race_plot$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(stc_race_plot$mapping$group),
    "~.config"
  )
  expect_equal(
    rlang::expr_text(stc_race_plot$mapping$colour),
    "~.config"
  )
  expect_snapshot(ggplot2::get_labs(stc_race_plot))

  # test autoplot --------------------------------------------------------------

  stc_autoplot <- autoplot(wl_static_res)

  expect_ptype(
    stc_autoplot$data,
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  )

  expect_equal(
    rlang::expr_text(stc_autoplot$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(stc_autoplot$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(stc_autoplot))

  # test metric collection -----------------------------------------------------

  exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )
  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  ###

  wl_finished <-
    map_dfr(wl_static_res$.metrics, I) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_static_res)

  expect_equal(nrow(wl_finished), nrow(metric_wl_sum))
  expect_ptype(metric_wl_sum, exp_metric_sum)
  expect_true(all(metric_wl_sum$.metric == "concordance_survival"))

  ###

  metric_wl_all <- collect_metrics(wl_static_res, summarize = FALSE)
  expect_true(nrow(metric_wl_all) == nrow(wl_finished) * nrow(sim_rs))
  expect_ptype(metric_wl_all, exp_metric_all)
  expect_true(all(metric_wl_all$.metric == "concordance_survival"))

  # test prediction collection -------------------------------------------------

  static_ptype <- tibble::tibble(
    .pred_time = numeric(0),
    id = character(0),
    .row = integer(0),
    cost_complexity = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  static_oob <-
    wl_static_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(wl_static_res)
  expect_ptype(unsum_pred, static_ptype)
  expect_equal(nrow(unsum_pred), static_oob * nrow(wl_finished))

  sum_pred <- collect_predictions(wl_static_res, summarize = TRUE)
  no_id <- static_ptype[, names(static_ptype) != "id"]
  expect_ptype(sum_pred, no_id)
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(wl_finished))

  # test metric collection pivoting --------------------------------------------

  skip_if_not_installed("tune", "1.1.2.9019")

  metric_all <- collect_metrics(wl_static_res, type = "wide")
  exp_metric_all <- tibble(
    cost_complexity = numeric(0),
    .config = character(0),
    concordance_survival = numeric(0)
  )

  expect_equal(metric_all %>% dplyr::slice(), exp_metric_all)
})

test_that("race tuning (win_loss) survival models with integrated metric", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

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
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  # make it so there will probably be 2+ winners
  grid <- tibble(cost_complexity = 10^c(-10.1, -10.0, -2.0, -1.0))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with integrated metrics ---------------------------------------------

  sint_mtrc <- metric_set(brier_survival_integrated)

  set.seed(2193)
  wl_integrated_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  expect_snapshot({
    num_final_wl <-
      show_best(
        wl_integrated_res,
        metric = "brier_survival_integrated",
        eval_time = 5
      ) %>%
      pluck("cost_complexity") %>%
      unique()
  })
  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(wl_integrated_res$.metrics[[1]]))

  expect_named(
    wl_integrated_res$.predictions[[1]],
    c(".pred", ".row", "cost_complexity", "event_time", ".config"),
    ignore.order = TRUE
  )

  expect_true(is.list(wl_integrated_res$.predictions[[1]]$.pred))

  expect_named(
    wl_integrated_res$.predictions[[1]]$.pred[[1]],
    c(".eval_time", ".pred_survival", ".weight_censored"),
    ignore.order = TRUE
  )

  expect_equal(
    wl_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test race plot -------------------------------------------------------------

  int_race_plot <- plot_race(wl_integrated_res)

  expect_ptype(
    int_race_plot$data,
    tibble::tibble(
      .config = character(0),
      mean = numeric(0),
      n = integer(0),
      stage = integer(0)
    )
  )

  expect_equal(
    rlang::expr_text(int_race_plot$mapping$x),
    "~stage"
  )
  expect_equal(
    rlang::expr_text(int_race_plot$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(int_race_plot$mapping$group),
    "~.config"
  )
  expect_equal(
    rlang::expr_text(int_race_plot$mapping$colour),
    "~.config"
  )
  expect_snapshot(ggplot2::get_labs(int_race_plot))

  # test autoplot --------------------------------------------------------------

  int_autoplot <- autoplot(wl_integrated_res)

  expect_ptype(
    int_autoplot$data,
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  )

  expect_equal(
    rlang::expr_text(int_autoplot$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(int_autoplot$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(int_autoplot))

  # test metric collection

  exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )
  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  ###

  wl_finished <-
    map_dfr(wl_integrated_res$.metrics, I) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_integrated_res)

  expect_equal(nrow(wl_finished), nrow(metric_wl_sum))
  expect_ptype(metric_wl_sum, exp_metric_sum)
  expect_true(all(metric_wl_sum$.metric == "brier_survival_integrated"))

  ###

  metric_wl_all <- collect_metrics(wl_integrated_res, summarize = FALSE)
  expect_true(nrow(metric_wl_all) == nrow(wl_finished) * nrow(sim_rs))
  expect_ptype(metric_wl_all, exp_metric_all)
  expect_true(all(metric_wl_all$.metric == "brier_survival_integrated"))

  # test prediction collection -------------------------------------------------

  integrated_ptype <- tibble::tibble(
    .pred = list(),
    id = character(0),
    .row = integer(0),
    cost_complexity = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  integrated_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  integrated_oob <-
    wl_integrated_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(wl_integrated_res)
  expect_ptype(unsum_pred, integrated_ptype)
  expect_equal(nrow(unsum_pred), integrated_oob * nrow(wl_finished))

  expect_ptype(unsum_pred$.pred[[1]], integrated_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(wl_integrated_res, summarize = TRUE)
  no_id <- integrated_ptype[, names(integrated_ptype) != "id"]
  expect_ptype(sum_pred, no_id)
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(wl_finished))

  expect_ptype(sum_pred$.pred[[1]], integrated_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test metric collection pivoting --------------------------------------------

  skip_if_not_installed("tune", "1.1.2.9019")

  metric_all <- collect_metrics(wl_integrated_res, type = "wide")
  exp_metric_all <- tibble(
    cost_complexity = numeric(0),
    .config = character(0),
    brier_survival_integrated = numeric(0)
  )

  expect_equal(metric_all %>% dplyr::slice(), exp_metric_all)
})

test_that("race tuning (win_loss) survival models with dynamic metrics", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

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
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid <- tibble(cost_complexity = 10^c(-10, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with dynamic metrics ------------------------------------------------

  dyn_mtrc <- metric_set(brier_survival)

  expect_snapshot({
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
  })

  num_final_wl <-
    unique(
      show_best(
        wl_dyn_res,
        metric = "brier_survival",
        eval_time = 5
      )$cost_complexity
    )

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(wl_dyn_res$.metrics[[1]]))

  expect_named(
    wl_dyn_res$.predictions[[1]],
    c(".pred", ".row", "cost_complexity", "event_time", ".config"),
    ignore.order = TRUE
  )

  expect_true(is.list(wl_dyn_res$.predictions[[1]]$.pred))

  expect_named(
    wl_dyn_res$.predictions[[1]]$.pred[[1]],
    c(".eval_time", ".pred_survival", ".weight_censored"),
    ignore.order = TRUE
  )

  expect_equal(
    wl_dyn_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test race plot -------------------------------------------------------------

  dyn_race_plot <- plot_race(wl_dyn_res)

  expect_ptype(
    dyn_race_plot$data,
    tibble::tibble(
      .config = character(0),
      mean = numeric(0),
      n = integer(0),
      stage = integer(0)
    )
  )

  expect_equal(
    rlang::expr_text(dyn_race_plot$mapping$x),
    "~stage"
  )
  expect_equal(
    rlang::expr_text(dyn_race_plot$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(dyn_race_plot$mapping$group),
    "~.config"
  )
  expect_equal(
    rlang::expr_text(dyn_race_plot$mapping$colour),
    "~.config"
  )
  expect_snapshot(ggplot2::get_labs(dyn_race_plot))

  # test autoplot --------------------------------------------------------------

  dyn_autoplot <- autoplot(wl_dyn_res)

  expect_ptype(
    dyn_autoplot$data,
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  )

  expect_equal(
    rlang::expr_text(dyn_autoplot$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(dyn_autoplot$mapping$y),
    "~mean"
  )
  expect_snapshot(ggplot2::get_labs(dyn_autoplot))

  # test metric collection -----------------------------------------------------

  exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0)
  )

  ###

  wl_finished <-
    map_dfr(wl_dyn_res$.metrics, I) %>%
    filter(.eval_time == 5) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_dyn_res)

  expect_equal(nrow(wl_finished) * length(time_points), nrow(metric_wl_sum))
  expect_ptype(metric_wl_sum, exp_metric_sum)
  expect_true(all(metric_wl_sum$.metric == "brier_survival"))

  ###

  metric_wl_all <- collect_metrics(wl_dyn_res, summarize = FALSE)
  expect_true(
    nrow(metric_wl_all) ==
      nrow(wl_finished) * nrow(sim_rs) * length(time_points)
  )
  expect_ptype(metric_wl_all, exp_metric_all)
  expect_true(all(metric_wl_all$.metric == "brier_survival"))

  # test prediction collection -------------------------------------------------

  dynamic_ptype <- tibble::tibble(
    .pred = list(),
    id = character(0),
    .row = integer(0),
    cost_complexity = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  dynamic_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  dyn_oob <-
    wl_dyn_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(wl_dyn_res)
  expect_ptype(unsum_pred, dynamic_ptype)
  expect_equal(nrow(unsum_pred), dyn_oob * nrow(wl_finished))

  expect_ptype(unsum_pred$.pred[[1]], dynamic_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(wl_dyn_res, summarize = TRUE)
  no_id <- dynamic_ptype[, names(dynamic_ptype) != "id"]
  expect_ptype(sum_pred, no_id)
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(wl_finished))

  expect_ptype(sum_pred$.pred[[1]], dynamic_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test metric collection pivoting --------------------------------------------

  skip_if_not_installed("tune", "1.1.2.9019")

  metric_all <- collect_metrics(wl_dyn_res, type = "wide")
  exp_metric_all <- tibble(
    cost_complexity = numeric(0),
    .config = character(0),
    .eval_time = numeric(0),
    brier_survival = numeric(0)
  )

  expect_equal(metric_all %>% dplyr::slice(), exp_metric_all)
})

test_that("race tuning (win_loss) survival models with linear_pred metric", {
  skip_if_not_installed("BradleyTerry2")
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
  sim_rs <- bootstraps(sim_tr, times = 20)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with linear_pred metrics --------------------------------------------

  linpred_mtrc <- metric_set(royston_survival)

  set.seed(2193)
  wl_linpred_res <-
    mod_spec %>%
    tune_race_win_loss(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = linpred_mtrc,
      control = rctrl
    )

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(wl_linpred_res$.metrics[[1]]))
  expect_named(
    wl_linpred_res$.predictions[[1]],
    c(".pred_linear_pred", ".row", "penalty", "event_time", ".config"),
    ignore.order = TRUE
  )

  # test metric collection -----------------------------------------------------

  exp_metric_sum <- tibble(
    penalty = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  wl_finished <-
    map_dfr(wl_linpred_res$.metrics, I) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_linpred_res)

  expect_equal(nrow(wl_finished), nrow(metric_wl_sum))
  expect_ptype(metric_wl_sum, exp_metric_sum)
  expect_true(all(metric_wl_sum$.metric == "royston_survival"))
})

test_that("race tuning (win_loss) survival models with mixture of metric types", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

  # standard setup start -------------------------------------------------------

  set.seed(1)
  sim_dat <- prodlim::SimSurv(500) %>%
    mutate(event_time = Surv(time, event)) %>%
    select(event_time, X1, X2)

  set.seed(2)
  split <- initial_split(sim_dat)
  sim_tr <- training(split)
  sim_te <- testing(split)
  sim_rs <- bootstraps(sim_tr, times = 30)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid_winner <- tibble(
    cost_complexity = 10^c(-10, seq(-1.1, -1, length.out = 5))
  )
  grid_ties <- tibble(cost_complexity = 10^c(seq(-10.1, -10.0, length.out = 5)))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with mixed metrics --------------------------------------------------

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival
  )

  expect_snapshot({
    set.seed(2193)
    wl_mixed_res <-
      mod_spec %>%
      tune_race_win_loss(
        event_time ~ X1 + X2,
        resamples = sim_rs,
        grid = grid_ties,
        metrics = mix_mtrc,
        eval_time = time_points,
        control = rctrl
      )
  })

  num_final_wl <- unique(
    show_best(
      wl_mixed_res,
      metric = "brier_survival",
      eval_time = 5
    )$cost_complexity
  )

  expect_equal(length(num_final_wl), nrow(grid_ties))

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(wl_mixed_res$.metrics[[1]]))

  expect_named(
    wl_mixed_res$.predictions[[1]],
    c(
      ".pred",
      ".row",
      "cost_complexity",
      ".pred_time",
      "event_time",
      ".config"
    ),
    ignore.order = TRUE
  )

  expect_true(is.list(wl_mixed_res$.predictions[[1]]$.pred))

  expect_named(
    wl_mixed_res$.predictions[[1]]$.pred[[1]],
    c(".eval_time", ".pred_survival", ".weight_censored"),
    ignore.order = TRUE
  )

  expect_equal(
    wl_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test race plot -------------------------------------------------------------

  mix_race_plot <- plot_race(wl_mixed_res)

  expect_ptype(
    mix_race_plot$data,
    tibble::tibble(
      .config = character(0),
      mean = numeric(0),
      n = integer(0),
      stage = integer(0)
    )
  )

  expect_equal(
    rlang::expr_text(mix_race_plot$mapping$x),
    "~stage"
  )
  expect_equal(
    rlang::expr_text(mix_race_plot$mapping$y),
    "~mean"
  )
  expect_equal(
    rlang::expr_text(mix_race_plot$mapping$group),
    "~.config"
  )
  expect_equal(
    rlang::expr_text(mix_race_plot$mapping$colour),
    "~.config"
  )
  expect_snapshot(ggplot2::get_labs(mix_race_plot))

  # test autoplot --------------------------------------------------------------

  mix_autoplot <- autoplot(wl_mixed_res)

  expect_ptype(
    mix_autoplot$data,
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  )

  expect_equal(
    rlang::expr_text(mix_autoplot$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(mix_autoplot$mapping$y),
    "~mean"
  )

  expect_equal(
    rlang::expr_text(mix_autoplot$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_autoplot$data$.metric)),
    c("brier_survival @10", "brier_survival_integrated", "concordance_survival")
  )

  expect_snapshot(ggplot2::get_labs(mix_autoplot))

  ###

  mix_multi_autoplot <- autoplot(wl_mixed_res, eval_time = c(1, 10))

  expect_ptype(
    mix_multi_autoplot$data,
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  )

  expect_equal(
    rlang::expr_text(mix_multi_autoplot$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(mix_multi_autoplot$mapping$y),
    "~mean"
  )

  expect_equal(
    rlang::expr_text(mix_multi_autoplot$facet$params$facets$.metric),
    "~.metric"
  )
  expect_equal(
    sort(unique(mix_multi_autoplot$data$.metric)),
    c(
      "brier_survival @ 1",
      "brier_survival @10",
      "brier_survival_integrated",
      "concordance_survival"
    )
  )

  expect_snapshot(ggplot2::get_labs(mix_multi_autoplot))

  ###

  mix_alt_autoplot <- autoplot(wl_mixed_res, metric = "concordance_survival")

  expect_ptype(
    mix_alt_autoplot$data,
    tibble::tibble(
      mean = numeric(0),
      `# resamples` = integer(0),
      .metric = character(0),
      name = character(0),
      value = numeric(0)
    )
  )

  expect_equal(
    rlang::expr_text(mix_alt_autoplot$mapping$x),
    "~value"
  )
  expect_equal(
    rlang::expr_text(mix_alt_autoplot$mapping$y),
    "~mean"
  )

  expect_snapshot(ggplot2::get_labs(mix_alt_autoplot))

  # test metric collection -----------------------------------------------------

  exp_metric_sum <- tibble(
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    mean = numeric(0),
    n = integer(0),
    std_err = numeric(0),
    .config = character(0)
  )

  exp_metric_all <- tibble(
    id = character(0),
    cost_complexity = numeric(0),
    .metric = character(0),
    .estimator = character(0),
    .eval_time = numeric(0),
    .estimate = numeric(0),
    .config = character(0)
  )
  num_metrics <- length(time_points) + 2

  ###

  wl_finished <-
    map_dfr(wl_mixed_res$.metrics, I) %>%
    filter(.eval_time == 5) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_wl_sum <- collect_metrics(wl_mixed_res)

  expect_equal(nrow(wl_finished) * num_metrics, nrow(metric_wl_sum))
  expect_ptype(metric_wl_sum, exp_metric_sum)
  expect_true(sum(is.na(metric_wl_sum$.eval_time)) == 2 * nrow(wl_finished))
  expect_equal(
    as.vector(table(metric_wl_sum$.metric)),
    c(4L, 1L, 1L) * nrow(wl_finished)
  )

  ###

  metric_wl_all <- collect_metrics(wl_mixed_res, summarize = FALSE)
  expect_true(
    nrow(metric_wl_all) == num_metrics * nrow(wl_finished) * nrow(sim_rs)
  )
  expect_ptype(metric_wl_all, exp_metric_all)
  expect_true(sum(is.na(metric_wl_sum$.eval_time)) == 2 * nrow(wl_finished))
  expect_equal(
    as.vector(table(metric_wl_sum$.metric)),
    c(4L, 1L, 1L) * nrow(wl_finished)
  )

  # test prediction collection -------------------------------------------------

  mixed_ptype <- tibble::tibble(
    .pred = list(),
    .pred_time = numeric(0),
    id = character(0),
    .row = integer(0),
    cost_complexity = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  mixed_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  mixed_oob <-
    wl_mixed_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(wl_mixed_res)
  expect_ptype(unsum_pred, mixed_ptype)
  expect_equal(nrow(unsum_pred), mixed_oob * nrow(wl_finished))

  expect_ptype(unsum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(wl_mixed_res, summarize = TRUE)
  no_id <- mixed_ptype[, names(mixed_ptype) != "id"]
  expect_ptype(sum_pred, no_id)
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(wl_finished))

  expect_ptype(sum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test show_best() -----------------------------------------------------------

  expect_snapshot(
    show_best(wl_mixed_res, metric = "brier_survival") %>%
      select(-.estimator, -.config)
  )
  expect_snapshot(
    show_best(wl_mixed_res, metric = "brier_survival", eval_time = 1) %>%
      select(-.estimator, -.config)
  )
  expect_snapshot(
    show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1.1)) %>%
      select(-.estimator, -.config),
    error = TRUE
  )
  expect_snapshot(
    show_best(wl_mixed_res, metric = "brier_survival", eval_time = c(1, 3)) %>%
      select(-.estimator, -.config)
  )
  expect_snapshot(
    res <- show_best(
      wl_mixed_res,
      metric = "unused_metric",
      eval_time = c(1, 3)
    ) %>%
      select(-.estimator, -.config),
    error = TRUE
  )
  expect_snapshot(
    show_best(wl_mixed_res, metric = "brier_survival_integrated") %>%
      select(-.estimator, -.config)
  )

  # test metric collection pivoting --------------------------------------------

  skip_if_not_installed("tune", "1.1.2.9019")

  metric_all <- collect_metrics(wl_mixed_res, type = "wide")
  exp_metric_all <- tibble(
    cost_complexity = numeric(0),
    .config = character(0),
    .eval_time = numeric(0),
    brier_survival = numeric(0),
    brier_survival_integrated = numeric(0),
    concordance_survival = numeric(0)
  )

  expect_equal(metric_all %>% dplyr::slice(), exp_metric_all)
})

test_that("race tuning (win_loss) survival models mixture of metric types including linear_pred", {
  # this is a separate to above because the other one uses a decision tree model
  # which does not give us the linear predictor
  skip_if_not_installed("BradleyTerry2")
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
  sim_rs <- bootstraps(sim_tr, times = 30)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    proportional_hazards(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet") %>%
    set_mode("censored regression")

  grid <- tibble(penalty = 10^c(-4, -2, -1))

  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with mixed metrics including linear_pred ----------------------------

  mix_mtrc <- metric_set(
    brier_survival,
    brier_survival_integrated,
    concordance_survival,
    royston_survival
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

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(wl_mixed_res$.metrics[[1]]))
  expect_named(
    wl_mixed_res$.predictions[[1]],
    c(
      ".pred",
      ".row",
      "penalty",
      ".pred_time",
      ".pred_linear_pred",
      "event_time",
      ".config"
    ),
    ignore.order = TRUE
  )

  # test metric collection -----------------------------------------------------

  wl_finished <-
    map_dfr(wl_mixed_res$.metrics, I) %>%
    filter(.eval_time == 5) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))

  metric_wl_sum <- collect_metrics(wl_mixed_res)
  num_metrics <- length(time_points) + 3

  expect_equal(nrow(wl_finished) * num_metrics, nrow(metric_wl_sum))
  expect_true("royston_survival" %in% metric_wl_sum$.metric)
  expect_true(sum(is.na(metric_wl_sum$.eval_time)) == 3 * nrow(wl_finished))

  # test prediction collection -------------------------------------------------

  mixed_ptype <- tibble::tibble(
    .pred = list(),
    .pred_time = numeric(0),
    .pred_linear_pred = numeric(0),
    id = character(0),
    .row = integer(0),
    penalty = numeric(0),
    event_time = survival::Surv(0, 1, type = "right")[FALSE],
    .config = character(0)
  )

  mixed_list_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0),
      .weight_censored = numeric(0)
    )

  mixed_oob <-
    wl_mixed_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(wl_mixed_res)
  expect_ptype(unsum_pred, mixed_ptype)
  expect_equal(nrow(unsum_pred), mixed_oob * nrow(wl_finished))

  expect_ptype(unsum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(wl_mixed_res, summarize = TRUE)
  no_id <- mixed_ptype[, names(mixed_ptype) != "id"]
  expect_ptype(sum_pred, no_id)
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(wl_finished))

  expect_ptype(sum_pred$.pred[[1]], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test show_best() -----------------------------------------------------------

  expect_snapshot(
    show_best(wl_mixed_res, metric = "brier_survival", eval_time = 1) %>%
      select(-.estimator, -.config)
  )
  expect_snapshot(
    show_best(wl_mixed_res, metric = "royston_survival", eval_time = 1) %>%
      select(-.estimator, -.config)
  )

  # test metric collection pivoting --------------------------------------------

  metric_all <- collect_metrics(wl_mixed_res, type = "wide")
  exp_metric_all <- tibble(
    penalty = numeric(0),
    .config = character(0),
    .eval_time = numeric(0),
    brier_survival = numeric(0),
    brier_survival_integrated = numeric(0),
    concordance_survival = numeric(0),
    royston_survival = numeric(0)
  )

  expect_equal(metric_all %>% dplyr::slice(), exp_metric_all)
})

test_that("race tuning (W/L) - unneeded eval_time", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

  lung_surv <- lung %>%
    mutate(surv = Surv(time, status), .keep = "unused")

  # mode is not censored regression
  set.seed(2193)
  expect_snapshot(
    tune_res <-
      linear_reg(penalty = tune(), engine = "glmnet") %>%
      tune_race_win_loss(
        mpg ~ .,
        resamples = vfold_cv(mtcars, 5),
        metrics = metric_set(rmse),
        eval_time = 10
      )
  )

  # static metric
  set.seed(2193)
  expect_snapshot(
    tune_res <-
      proportional_hazards(penalty = tune(), engine = "glmnet") %>%
      tune_race_win_loss(
        surv ~ .,
        resamples = vfold_cv(lung_surv, 5),
        metrics = metric_set(concordance_survival),
        eval_time = 10
      )
  )
})
