test_that("last_fit() is reproducible", {
  skip_if_not_installed("tune", minimum_version = "1.3.0.9001")
  skip_if_not_installed("ranger")

  # ------------------------------------------------------------------------------

  set.seed(1)
  dat <- sim_regression(200)
  split <- initial_split(dat)
  tr_dat <- training(split)
  te_dat <- testing(split)
  rs <- vfold_cv(tr_dat)

  rf_spec <- rand_forest(mode = "regression", trees = 20)
  rf_wflow <- workflow(outcome ~ ., rf_spec)

  # ------------------------------------------------------------------------------

  # manual:

  set.seed(2)
  manual_fit <- fit(rf_wflow, tr_dat)
  manual_pred <-
    predict(manual_fit, new_data = te_dat)

  # auto:

  set.seed(2)
  auto_res <- last_fit(rf_wflow, split)
  auto_pred <-
    auto_res |>
    collect_predictions() |>
    select(outcome, auto = .pred)

  # tune version 1.3.0:
  both_pred <- full_join(manual_pred, auto_pred, by = "outcome")
  expect_equal(both_pred$auto, both_pred$manual)

})
