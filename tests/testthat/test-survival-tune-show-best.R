
test_that("show_best with censored data - integrated metric - grid", {

  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9007")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9010")

  obj <- make_churn_cens_objects()

  g_ctrl <- control_grid(save_pred = TRUE)

  tree_spec <-
    decision_tree(cost_complexity = tune(), min_n = 2) %>%
    set_mode("censored regression")

  int_met <- metric_set(brier_survival_integrated)

  set.seed(1)
  grid_int_res <-
    tree_spec %>%
    tune_grid(
      event_time ~ .,
      resamples = obj$rs,
      grid = tibble(cost_complexity = 10^seq(-4, -2, by = .1)),
      control = g_ctrl,
      metrics = int_met,
      eval_time = obj$times
    )

  expect_equal(
    show_best(grid_int_res, metric = "brier_survival_integrated"),
    grid_int_res %>%
      collect_metrics() %>%
      arrange(mean) %>%
      slice_min(mean, n = 5)
  )
  expect_snapshot(
    show_best(grid_int_res)
  )

})


test_that("show_best with censored data - dynamic metric - bayes", {

  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9007")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9005")

  obj <- make_churn_cens_objects()

  tree_spec <-
    decision_tree(cost_complexity = tune(), min_n = 2) %>%
    set_mode("censored regression")

  dyn_met <- metric_set(brier_survival)

  set.seed(611)
  bayes_dyn_res <-
    tree_spec %>%
    tune_bayes(
      event_time ~ .,
      resamples = obj$rs,
      initial = 4,
      iter = 3,
      metrics = dyn_met,
      eval_time = 100
    )

  expect_equal(
    show_best(bayes_dyn_res, metric = "brier_survival", eval_time = 100, n = 2),
    bayes_dyn_res %>%
      collect_metrics() %>%
      arrange(mean) %>%
      slice(1:2)
  )
  expect_snapshot(
    show_best(bayes_dyn_res)
  )
  expect_snapshot(
    show_best(bayes_dyn_res, metric = "brier_survival", eval_time = 1),
    error = TRUE
  )
  expect_snapshot(
    show_best(bayes_dyn_res, metric = "brier_survival_integrated"),
    error = TRUE
  )

})


test_that("show_best with censored data - static metric - anova racing", {

  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9007")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9005")
  skip_if_not_installed("finetune", minimum_version = "1.1.0.9004")

  obj <- make_churn_cens_objects()
  suppressPackageStartupMessages(library("finetune"))

  tree_spec <-
    decision_tree(cost_complexity = tune(), min_n = 2) %>%
    set_mode("censored regression")

  stc_met <- metric_set(concordance_survival)

  set.seed(22)
  race_stc_res <-
    tree_spec %>%
    tune_race_anova(
      event_time ~ .,
      resamples = obj$rs,
      grid = tibble(cost_complexity = 10^c(-1.4, -2.5, -3, -5)),
      metrics = stc_met
    )

  num_rs <- nrow(obj$rs)
  winner <-
    race_stc_res %>%
    collect_metrics(summarize = FALSE) %>%
    count(.config) %>%
    filter(n == num_rs) %>%
    arrange(.config) %>%
    slice(1) %>%
    pluck(".config")

  expect_equal(
    sort(show_best(race_stc_res, metric = "concordance_survival", n = 1)$.config),
    winner
  )
  expect_snapshot(
    show_best(race_stc_res)
  )
  expect_snapshot(
    show_best(race_stc_res, metric = "concordance_survival", eval_time = 1)
  )
  expect_snapshot(
    show_best(race_stc_res, metric = "brier_survival_integrated"),
    error = TRUE
  )

})


test_that("show_best with censored data - static metric (+dyn) - W/L racing", {

  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9007")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9005")
  skip_if_not_installed("finetune", minimum_version = "1.1.0.9004")

  obj <- make_churn_cens_objects()
  suppressPackageStartupMessages(library("finetune"))

  tree_spec <-
    decision_tree(cost_complexity = tune(), min_n = 2) %>%
    set_mode("censored regression")

  tree_param <-
    tree_spec %>%
    extract_parameter_set_dials() %>%
    update(cost_complexity = cost_complexity(c(-5, -1)))

  surv_met <- metric_set(concordance_survival, brier_survival)

  set.seed(326)
  race_stc_res <-
    tree_spec %>%
    tune_race_win_loss(
      event_time ~ .,
      resamples = obj$rs,
      grid = 10,
      metrics = surv_met,
      eval_time = 100,
      param_info = tree_param
    )

  num_rs <- nrow(obj$rs)
  winners <-
    race_stc_res %>%
    collect_metrics() %>%
    filter(.metric == "concordance_survival" & n == num_rs) %>%
    arrange(desc(mean)) %>%
    slice(1:5) %>%
    pluck(".config")

  expect_equal(
    show_best(race_stc_res, metric = "concordance_survival")$.config,
    winners
  )
  expect_snapshot(
    show_best(race_stc_res)
  )
  expect_snapshot(
    show_best(race_stc_res, metric = "concordance_survival", eval_time = 1)
  )
  expect_snapshot(
    show_best(race_stc_res, metric = "brier_survival_integrated"),
    error = TRUE
  )

})


test_that("show_best with censored data - dyn metric (+stc) - W/L racing", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9007")
  skip_if_not_installed("tune", minimum_version = "1.1.2.9005")
  skip_if_not_installed("finetune", minimum_version = "1.1.0.9004")

  obj <- make_churn_cens_objects()
  suppressPackageStartupMessages(library("finetune"))

  boost_spec <-
    boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  tree_spec <-
    decision_tree(cost_complexity = tune(), min_n = 2) %>%
    set_mode("censored regression")

  tree_param <-
    tree_spec %>%
    extract_parameter_set_dials() %>%
    update(cost_complexity = cost_complexity(c(-5, -1)))

  surv_met <- metric_set(brier_survival, concordance_survival)

  set.seed(326)
  race_dyn_res <-
    tree_spec %>%
    tune_race_win_loss(
      event_time ~ .,
      resamples = obj$rs,
      grid = 10,
      metrics = surv_met,
      eval_time = 100,
      param_info = tree_param
    )

  num_rs <- nrow(obj$rs)
  winners <-
    race_dyn_res %>%
    collect_metrics() %>%
    filter(.metric == "brier_survival" & n == num_rs) %>%
    arrange(mean) %>%
    slice(1:5) %>%
    pluck(".config")

  expect_equal(
    show_best(race_dyn_res, metric = "brier_survival")$.config,
    winners
  )
  expect_snapshot(
    show_best(race_dyn_res)
  )
  expect_snapshot(
    show_best(race_dyn_res, metric = "concordance_survival")
  )
  expect_snapshot(
    show_best(race_dyn_res, metric = "brier_survival", eval_time = 1),
    error = TRUE
  )
  expect_snapshot(
    show_best(race_dyn_res, metric = "brier_survival_integrated"),
    error = TRUE
  )

})

