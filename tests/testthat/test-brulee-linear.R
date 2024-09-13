test_that("linear regression via brulee", {
  skip_if_not_installed("torch")
  skip_if_not(any(get_from_env("linear_reg")$engine == "brulee"))
  skip_if_not(is_torch_working())

  set.seed(2832)
  ols_fit <-
    linear_reg() %>%
    set_engine("brulee", epochs = 2) %>%
    fit(outcome ~ ., data = num_tr)
  expect_s3_class(ols_fit, c("_brulee_linear_reg", "model_fit"))
  ols_pred <- predict(ols_fit, num_te)
  expect_true(inherits(ols_pred, "data.frame"))
  expect_true(nrow(ols_pred) == 2)
  expect_named(ols_pred, ".pred")

  # ------------------------------------------------------------------------------

  lr_spec <-
    linear_reg(penalty = tune()) %>% #TODO  mixture = tune()
    set_engine("brulee", epochs = tune(), learn_rate = tune(), stop_iter = tune()) %>%
    set_mode("regression")

  lr_param <-
    lr_spec %>%
    extract_parameter_set_dials() %>%
    update(
      epochs = epochs(c(1, 10))
    )

  set.seed(487)
  lr_res <-
   lr_spec %>%
    tune_grid(
      outcome ~ .,
      num_rs,
      grid = 2,
      param_info = lr_param)

  expect_true(nrow(collect_notes(lr_res)) == 0)

})

