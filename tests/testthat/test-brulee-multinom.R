test_that("multinomial regression via brulee", {
  skip_if_not_installed("torch")
  skip_if_not(any(get_from_env("multinom_reg")$engine == "brulee"))
  skip_if_not(is_torch_working())

  set.seed(2832)
  mnl_fit <-
    multinom_reg() %>%
    set_engine("brulee", epochs = 2, class_weights = 1/2) %>%
    fit(class ~ ., data = three_class_tr)
  expect_s3_class(mnl_fit, c("_brulee_multinom_reg", "model_fit"))

  mnl_class_pred <- predict(mnl_fit, three_class_te, type = "class")
  expect_true(inherits(mnl_class_pred, "data.frame"))
  expect_true(nrow(mnl_class_pred) == 2)
  expect_named(mnl_class_pred, ".pred_class")

  mnl_prob_pred <- predict(mnl_fit, three_class_te, type = "prob")
  expect_true(inherits(mnl_prob_pred, "data.frame"))
  expect_true(nrow(mnl_prob_pred) == 2)
  expect_named(mnl_prob_pred, c(".pred_one", ".pred_two", ".pred_three"))


  # ------------------------------------------------------------------------------

  mnl_spec <-
    multinom_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("brulee", epochs = tune(), learn_rate = tune(),
               stop_iter = tune(), class_weights = tune()) %>%
    set_mode("classification")

  mnl_param <-
    mnl_spec %>%
    extract_parameter_set_dials() %>%
    update(
      epochs = epochs(c(1, 10))
    )

  set.seed(217)
  mnl_res <-
    mnl_spec %>%
    tune_grid(
      class ~ .,
      three_class_rs,
      grid = 2,
      param_info = mnl_param)

  expect_true(nrow(collect_notes(mnl_res)) == 0)

})
