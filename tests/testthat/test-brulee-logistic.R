test_that("logistic regression via brulee", {
  skip_if_not_installed("torch")
  skip_if_not(any(get_from_env("logistic_reg")$engine == "brulee"))
  skip_if_not(is_torch_working())

  set.seed(2832)
  glm_fit <-
    logistic_reg() %>%
    set_engine("brulee", epochs = 2, class_weights = 1/2) %>%
    fit(class ~ ., data = binary_tr)
  expect_s3_class(glm_fit, c("_brulee_logistic_reg", "model_fit"))

  glm_class_pred <- predict(glm_fit, binary_te, type = "class")
  expect_true(inherits(glm_class_pred, "data.frame"))
  expect_true(nrow(glm_class_pred) == 2)
  expect_named(glm_class_pred, ".pred_class")

  glm_prob_pred <- predict(glm_fit, binary_te, type = "prob")
  expect_true(inherits(glm_prob_pred, "data.frame"))
  expect_true(nrow(glm_prob_pred) == 2)
  expect_named(glm_prob_pred, c(".pred_one", ".pred_two"))


  # ------------------------------------------------------------------------------

  lr_spec <-
    logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("brulee", epochs = tune(), learn_rate = tune(),
               stop_iter = tune(), class_weights = tune()) %>%
    set_mode("classification")

  lr_param <-
    lr_spec %>%
    extract_parameter_set_dials() %>%
    update(
      epochs = epochs(c(1, 10))
    )

  set.seed(473)
  lr_res <-
    lr_spec %>%
    tune_grid(
      class ~ .,
      binary_rs,
      grid = 2,
      param_info = lr_param)

  expect_true(nrow(collect_notes(lr_res)) == 0)

})
