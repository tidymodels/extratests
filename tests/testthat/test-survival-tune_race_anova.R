suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(censored))
suppressPackageStartupMessages(library(finetune))

skip_if_not_installed("parsnip", minimum_version = "1.1.0.9003")
skip_if_not_installed("censored", minimum_version = "0.2.0.9000")
skip_if_not_installed("tune", minimum_version = "1.1.2.9012")
skip_if_not_installed("yardstick", minimum_version = "1.2.0.9001")
skip_if_not_installed("finetune", minimum_version = "1.1.0.9005")

test_that("race tuning (anova) survival models with static metric", {
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
  # high-ish number of bootstraps to simulate a case where the race gets down to
  # a single configuration
  sim_rs <- bootstraps(sim_tr, times = 20)

  time_points <- c(10, 1, 5, 15)

  mod_spec <-
    decision_tree(cost_complexity = tune()) %>%
    set_mode("censored regression")

  grid <- tibble(cost_complexity = 10^c(-10, -2, -1))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)

  # Racing with static metrics -------------------------------------------------

  stc_mtrc  <- metric_set(concordance_survival)

  set.seed(2193)
  aov_static_res <-
    mod_spec %>%
    tune_race_anova(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = stc_mtrc,
      control = rctrl
    )

  num_final_aov <-
    unique(show_best(aov_static_res, metric = "concordance_survival")$cost_complexity)

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(aov_static_res$.metrics[[1]]))

  expect_equal(
    names(aov_static_res$.predictions[[1]]),
    c(".pred_time", ".row", "cost_complexity", "event_time", ".config")
  )

  # test race plot -------------------------------------------------------------

  stc_race_plot <- plot_race(aov_static_res)

  expect_equal(
    stc_race_plot$data[0,],
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
  expect_equal(
    stc_race_plot$labels,
    list(y = "concordance_survival", x = "Analysis Stage", group = ".config",
         colour = ".config")
  )

  # test autoplot --------------------------------------------------------------

  stc_autoplot <- autoplot(aov_static_res)

  expect_equal(
    stc_autoplot$data[0,],
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
  expect_equal(
    stc_autoplot$labels,
    list(x = c(cost_complexity = "Cost-Complexity Parameter"),
         y = "concordance_survival",  alpha = "# resamples", size = "# resamples")
  )

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

  aov_finished <-
    map_dfr(aov_static_res$.metrics, I) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_aov_sum <- collect_metrics(aov_static_res)

  expect_equal(nrow(aov_finished), nrow(metric_aov_sum))
  expect_equal(metric_aov_sum[0,], exp_metric_sum)
  expect_true(all(metric_aov_sum$.metric == "concordance_survival"))

  ###

  metric_aov_all <- collect_metrics(aov_static_res, summarize = FALSE)
  expect_true(nrow(metric_aov_all) == nrow(aov_finished) * nrow(sim_rs))
  expect_equal(metric_aov_all[0,], exp_metric_all)
  expect_true(all(metric_aov_all$.metric == "concordance_survival"))

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
    aov_static_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(aov_static_res)
  expect_equal(unsum_pred[0,], static_ptype)
  expect_equal(nrow(unsum_pred), static_oob * nrow(aov_finished))

  sum_pred <- collect_predictions(aov_static_res, summarize = TRUE)
  expect_equal(sum_pred[0,], static_ptype[, names(static_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(aov_finished))

})

test_that("race tuning (anova) survival models with integrated metric", {
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
  aov_integrated_res <-
    mod_spec %>%
    tune_race_anova(
      event_time ~ X1 + X2,
      resamples = sim_rs,
      grid = grid,
      metrics = sint_mtrc,
      eval_time = time_points,
      control = rctrl
    )

  expect_snapshot({
    num_final_aov <-
      show_best(aov_integrated_res, metric = "brier_survival_integrated", eval_time = 5) %>%
      pluck("cost_complexity") %>%
      unique()
  })

  # test structure of results --------------------------------------------------

  expect_false(".eval_time" %in% names(aov_integrated_res$.metrics[[1]]))

  expect_equal(
    names(aov_integrated_res$.predictions[[1]]),
    c(".pred", ".row", "cost_complexity", "event_time", ".config")
  )

  expect_true(is.list(aov_integrated_res$.predictions[[1]]$.pred))

  expect_equal(
    names(aov_integrated_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    aov_integrated_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test race plot -------------------------------------------------------------

  int_race_plot <- plot_race(aov_integrated_res)

  expect_equal(
    int_race_plot$data[0,],
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
  expect_equal(
    int_race_plot$labels,
    list(y = "brier_survival_integrated", x = "Analysis Stage", group = ".config",
         colour = ".config")
  )

  # test autoplot --------------------------------------------------------------

  int_autoplot <- autoplot(aov_integrated_res)

  expect_equal(
    int_autoplot$data[0,],
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
  expect_equal(
    int_autoplot$labels,
    list(x = c(cost_complexity = "Cost-Complexity Parameter"),
         y = "brier_survival_integrated",  alpha = "# resamples", size = "# resamples")
  )

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

  aov_finished <-
    map_dfr(aov_integrated_res$.metrics, I) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_aov_sum <- collect_metrics(aov_integrated_res)

  expect_equal(nrow(aov_finished), nrow(metric_aov_sum))
  expect_equal(metric_aov_sum[0,], exp_metric_sum)
  expect_true(all(metric_aov_sum$.metric == "brier_survival_integrated"))

  ###

  metric_aov_all <- collect_metrics(aov_integrated_res, summarize = FALSE)
  expect_true(nrow(metric_aov_all) == nrow(aov_finished) * nrow(sim_rs))
  expect_equal(metric_aov_all[0,], exp_metric_all)
  expect_true(all(metric_aov_all$.metric == "brier_survival_integrated"))

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
    aov_integrated_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(aov_integrated_res)
  expect_equal(unsum_pred[0,], integrated_ptype)
  expect_equal(nrow(unsum_pred), integrated_oob * nrow(aov_finished))

  expect_equal(unsum_pred$.pred[[1]][0,], integrated_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))


  sum_pred <- collect_predictions(aov_integrated_res, summarize = TRUE)
  expect_equal(sum_pred[0,], integrated_ptype[, names(integrated_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(aov_finished))

  expect_equal(sum_pred$.pred[[1]][0,], integrated_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))
})

test_that("race tuning (anova) survival models with dynamic metrics", {
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
  rctrl_verb <- control_race(save_pred = TRUE, verbose_elim = TRUE, verbose = FALSE)

  # Racing with dynamic metrics ------------------------------------------------

  dyn_mtrc  <- metric_set(brier_survival)

  # use `capture.output()` instead of `expect_snapshot_test()`
  # https://github.com/tidymodels/extratests/pull/134#discussion_r1394534647
  # expect_snapshot_warning() is used so that we can test on the captured output
  expect_snapshot_warning({
    aov_dyn_output <-
      capture.output({
        set.seed(2193)
        aov_dyn_res <-
          mod_spec %>%
          tune_race_anova(
            event_time ~ X1 + X2,
            resamples = sim_rs,
            grid = grid,
            metrics = dyn_mtrc,
            eval_time = time_points,
            control = rctrl_verb
          )
      },
      type = "message")
  })

  num_final_aov <-
    show_best(aov_dyn_res, metric = "brier_survival", eval_time = 5) %>%
    pluck("cost_complexity") %>%
    unique()

  # TODO add a test for checking the evaluation time in this message:
  # https://github.com/tidymodels/finetune/issues/81
  expect_true(any(grepl("Racing will minimize the brier_survival metric at time", aov_dyn_output)))

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(aov_dyn_res$.metrics[[1]]))

  expect_equal(
    names(aov_dyn_res$.predictions[[1]]),
    c(".pred", ".row", "cost_complexity", "event_time", ".config")
  )

  expect_true(is.list(aov_dyn_res$.predictions[[1]]$.pred))

  expect_equal(
    names(aov_dyn_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    aov_dyn_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )

  # test race plot -------------------------------------------------------------

  dyn_race_plot <- plot_race(aov_dyn_res)

  expect_equal(
    dyn_race_plot$data[0,],
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
  expect_equal(
    dyn_race_plot$labels,
    list(y = "brier_survival", x = "Analysis Stage", group = ".config",
         colour = ".config")
  )

  # test autoplot --------------------------------------------------------------

  dyn_autoplot <- autoplot(aov_dyn_res)

  expect_equal(
    dyn_autoplot$data[0,],
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
  expect_equal(
    dyn_autoplot$labels,
    list(x = c(cost_complexity = "Cost-Complexity Parameter"),
         y = "brier_survival @10",  alpha = "# resamples", size = "# resamples")
  )


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

  aov_finished <-
    map_dfr(aov_dyn_res$.metrics, I) %>%
    filter(.eval_time == 5) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_aov_sum <- collect_metrics(aov_dyn_res)

  expect_equal(nrow(aov_finished) * length(time_points), nrow(metric_aov_sum))
  expect_equal(metric_aov_sum[0,], exp_metric_sum)
  expect_true(all(metric_aov_sum$.metric == "brier_survival"))

  ###

  metric_aov_all <- collect_metrics(aov_dyn_res, summarize = FALSE)
  expect_true(nrow(metric_aov_all) == nrow(aov_finished) * nrow(sim_rs) * length(time_points))
  expect_equal(metric_aov_all[0,], exp_metric_all)
  expect_true(all(metric_aov_all$.metric == "brier_survival"))

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
    aov_dyn_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(aov_dyn_res)
  expect_equal(unsum_pred[0,], dynamic_ptype)
  expect_equal(nrow(unsum_pred), dyn_oob * nrow(aov_finished))

  expect_equal(unsum_pred$.pred[[1]][0,], dynamic_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))


  sum_pred <- collect_predictions(aov_dyn_res, summarize = TRUE)
  expect_equal(sum_pred[0,], dynamic_ptype[, names(dynamic_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(aov_finished))

  expect_equal(sum_pred$.pred[[1]][0,], dynamic_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

})

test_that("race tuning (anova) survival models with mixture of metric types", {
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

  grid_winner <- tibble(cost_complexity = 10^c(-10, seq(-1.1, -1, length.out = 5)))
  grid_ties <- tibble(cost_complexity = 10^c(seq(-10.1, -10.0, length.out = 5)))

  gctrl <- control_grid(save_pred = TRUE)
  rctrl <- control_race(save_pred = TRUE, verbose_elim = FALSE, verbose = FALSE)
  rctrl_verb <- control_race(save_pred = TRUE, verbose_elim = TRUE, verbose = FALSE)

  # Racing with mixed metrics --------------------------------------------------

  mix_mtrc  <- metric_set(brier_survival, brier_survival_integrated, concordance_survival)

  # expect_snapshot_warning() is used so that we can test on the captured output
  expect_snapshot_warning({
    aov_mixed_output <-
      capture.output({
        set.seed(2193)
        aov_mixed_res <-
          mod_spec %>%
          tune_race_anova(
            event_time ~ X1 + X2,
            resamples = sim_rs,
            grid = grid_winner,
            metrics = mix_mtrc,
            eval_time = time_points,
            control = rctrl_verb
          )
      },
      type = "message")
  })

  num_final_aov <- unique(show_best(aov_mixed_res, metric = "brier_survival", eval_time = 5)$cost_complexity)

  expect_true(any(grepl("Racing will minimize the brier_survival metric at time 10", aov_mixed_output)))
  expect_true(length(num_final_aov) < nrow(grid_winner))

  # test structure of results --------------------------------------------------

  expect_true(".eval_time" %in% names(aov_mixed_res$.metrics[[1]]))

  expect_equal(
    names(aov_mixed_res$.predictions[[1]]),
    c(".pred", ".row", "cost_complexity", ".pred_time", "event_time", ".config")
  )

  expect_true(is.list(aov_mixed_res$.predictions[[1]]$.pred))

  expect_equal(
    names(aov_mixed_res$.predictions[[1]]$.pred[[1]]),
    c(".eval_time", ".pred_survival", ".weight_censored")
  )

  expect_equal(
    aov_mixed_res$.predictions[[1]]$.pred[[1]]$.eval_time,
    time_points
  )


  # test race plot -------------------------------------------------------------

  mix_race_plot <- plot_race(aov_mixed_res)

  expect_equal(
    mix_race_plot$data[0,],
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
  expect_equal(
    mix_race_plot$labels,
    list(y = "brier_survival", x = "Analysis Stage", group = ".config",
         colour = ".config")
  )

  # test autoplot --------------------------------------------------------------

  mix_autoplot <- autoplot(aov_mixed_res)

  expect_equal(
    mix_autoplot$data[0,],
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
    c("brier_survival @10",
      "brier_survival_integrated",
      "concordance_survival")
  )

  expect_equal(
    mix_autoplot$labels,
    list(x = c(cost_complexity = "Cost-Complexity Parameter"),
         y = "",  alpha = "# resamples", size = "# resamples")
  )


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

  aov_finished <-
    map_dfr(aov_mixed_res$.metrics, I) %>%
    filter(.eval_time == 5) %>%
    count(.config) %>%
    filter(n == nrow(sim_rs))
  metric_aov_sum <- collect_metrics(aov_mixed_res)

  expect_equal(nrow(aov_finished) * num_metrics, nrow(metric_aov_sum))
  expect_equal(metric_aov_sum[0,], exp_metric_sum)
  expect_true(sum(is.na(metric_aov_sum$.eval_time)) == 2 * nrow(aov_finished))
  expect_equal(as.vector(table(metric_aov_sum$.metric)), c(4L, 1L, 1L) * nrow(aov_finished))

  ###

  metric_aov_all <- collect_metrics(aov_mixed_res, summarize = FALSE)
  expect_true(nrow(metric_aov_all) == num_metrics * nrow(aov_finished) * nrow(sim_rs))
  expect_equal(metric_aov_all[0,], exp_metric_all)
  expect_true(sum(is.na(metric_aov_sum$.eval_time)) == 2 * nrow(aov_finished))
  expect_equal(as.vector(table(metric_aov_sum$.metric)), c(4L, 1L, 1L) * nrow(aov_finished))

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
    aov_mixed_res %>%
    mutate(oob = map_int(splits, ~ nrow(assessment(.x)))) %>%
    pluck("oob") %>%
    sum()

  unsum_pred <- collect_predictions(aov_mixed_res)
  expect_equal(unsum_pred[0,], mixed_ptype)
  expect_equal(nrow(unsum_pred), mixed_oob * nrow(aov_finished))

  expect_equal(unsum_pred$.pred[[1]][0,], mixed_list_ptype)
  expect_equal(nrow(unsum_pred$.pred[[1]]), length(time_points))

  sum_pred <- collect_predictions(aov_mixed_res, summarize = TRUE)
  expect_equal(sum_pred[0,], mixed_ptype[, names(mixed_ptype) != "id"])
  expect_equal(nrow(sum_pred), nrow(sim_tr) * nrow(aov_finished))

  expect_equal(sum_pred$.pred[[1]][0,], mixed_list_ptype)
  expect_equal(nrow(sum_pred$.pred[[1]]), length(time_points))

  # test show_best() -----------------------------------------------------------

  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival") %>%
      select(-.estimator, -.config)
    )
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival", eval_time = 1) %>%
      select(-.estimator, -.config)
    )
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1.1)) %>%
      select(-.estimator, -.config),
    error = TRUE
  )
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival", eval_time = c(1, 3)) %>%
      select(-.estimator, -.config),
  )
  expect_snapshot(
    show_best(aov_mixed_res, metric = "brier_survival_integrated") %>%
      select(-.estimator, -.config)
  )
})

test_that("race tuning (anova) - unneeded eval_time", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("flexsurv")

  lung_surv <- lung %>%
    mutate(surv = Surv(time, status), .keep = "unused")

  # mode is not censored regression
  set.seed(2193)
  expect_snapshot(
    tune_res <-
      linear_reg(penalty = tune(), engine = "glmnet") %>%
      tune_race_anova(
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
      tune_race_anova(
        surv ~ .,
        resamples = vfold_cv(lung_surv, 5),
        metrics = metric_set(concordance_survival),
        eval_time = 10
      )
  )
})
