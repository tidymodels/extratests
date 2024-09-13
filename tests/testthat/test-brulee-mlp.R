test_that("classification neural network via brulee", {
  skip_if_not_installed("torch")
  skip_if_not(any(get_from_env("mlp")$engine == "brulee"))
  skip_if_not(is_torch_working())

  set.seed(232)
  nnet_fit <-
    mlp(hidden_units = 2, learn_rate = 0.01, epochs = 2) %>%
    set_engine("brulee", class_weights = 1/2) %>%
    set_mode("classification") %>%
    fit(class ~ ., data = three_class_tr)
  expect_s3_class(nnet_fit, c("_brulee_mlp", "model_fit"))

  nnet_class_pred <- predict(nnet_fit, three_class_te, type = "class")
  expect_true(inherits(nnet_class_pred, "data.frame"))
  expect_true(nrow(nnet_class_pred) == 2)
  expect_named(nnet_class_pred, ".pred_class")

  nnet_prob_pred <- predict(nnet_fit, three_class_te, type = "prob")
  expect_true(inherits(nnet_prob_pred, "data.frame"))
  expect_true(nrow(nnet_prob_pred) == 2)
  expect_named(nnet_prob_pred, c(".pred_one", ".pred_two", ".pred_three"))

})

test_that("regression neural network via brulee", {
  skip_if_not_installed("torch")
  skip_if_not(any(get_from_env("mlp")$engine == "brulee"))
  skip_if_not(is_torch_working())

  set.seed(2832)
  nnet_fit <-
    mlp(hidden_units = 2, learn_rate = 0.01, epochs = 2) %>%
    set_engine("brulee") %>%
    set_mode("regression") %>%
    fit(outcome ~ ., data = num_tr)
  expect_s3_class(nnet_fit, c("_brulee_mlp", "model_fit"))

  nnet_class_pred <- predict(nnet_fit, num_te)
  expect_true(inherits(nnet_class_pred, "data.frame"))
  expect_true(nrow(nnet_class_pred) == 2)
  expect_named(nnet_class_pred, ".pred")

  # ------------------------------------------------------------------------------

  nnet_spec <-
    mlp(hidden_units = tune(), learn_rate = tune(), epochs = tune(),
        penalty = tune(), activation = tune()) %>%
    set_engine("brulee", stop_iter = tune()) %>%
    set_mode("regression")

  nnet_param <-
    nnet_spec %>%
    extract_parameter_set_dials() %>%
    update(
      hidden_units = hidden_units(c(2, 5)),
      epochs = epochs(c(1, 10))
    )

  set.seed(682)
  nnet_res <-
    nnet_spec %>%
    tune_grid(
      outcome ~ .,
      num_rs,
      grid = 2,
      param_info = nnet_param)

  expect_true(nrow(collect_notes(nnet_res)) == 0)
})


test_that("classification neural network (2 hidden layers) via brulee", {
  skip_if_not_installed("torch")
  skip_if_not(any(get_from_env("mlp")$engine == "brulee_two_layer"))
  skip_if_not(is_torch_working())

  set.seed(28132)
  nnet_fit <-
    mlp(hidden_units = 2, learn_rate = 0.01, epochs = 2) %>%
    set_engine("brulee_two_layer", class_weights = 1/2, hidden_units_2 = 3,
               activation_2 = "elu") %>%
    set_mode("classification") %>%
    fit(class ~ ., data = three_class_tr)
  expect_s3_class(nnet_fit, c("_brulee_two_layer_mlp", "model_fit"))

  nnet_class_pred <- predict(nnet_fit, three_class_te, type = "class")
  expect_true(inherits(nnet_class_pred, "data.frame"))
  expect_true(nrow(nnet_class_pred) == 2)
  expect_named(nnet_class_pred, ".pred_class")

  nnet_prob_pred <- predict(nnet_fit, three_class_te, type = "prob")
  expect_true(inherits(nnet_prob_pred, "data.frame"))
  expect_true(nrow(nnet_prob_pred) == 2)
  expect_named(nnet_prob_pred, c(".pred_one", ".pred_two", ".pred_three"))

  # ------------------------------------------------------------------------------

  nnet_spec <-
    mlp(hidden_units = tune(), learn_rate = tune(), epochs = tune(),
        penalty = tune(), activation = tune()) %>%
    set_engine("brulee_two_layer", class_weights = tune(),
               hidden_units_2 = tune(),
               activation_2 = tune()) %>%
    set_mode("classification")

  nnet_param <-
    nnet_spec %>%
    extract_parameter_set_dials() %>%
    update(
      hidden_units = hidden_units(c(2, 5)),
      hidden_units_2 = hidden_units_2(c(2, 5)),
      epochs = epochs(c(1, 10))
    )

  set.seed(28132)
  nnet_res <-
    nnet_spec %>%
    tune_grid(
      class ~ .,
      binary_rs,
      grid = 2,
      param_info = nnet_param)
  expect_true(nrow(collect_notes(nnet_res)) == 0)

})

test_that("regression neural network (2 hidden layers) via brulee", {
  skip_if_not_installed("torch")
  skip_if_not(any(get_from_env("mlp")$engine == "brulee_two_layer"))
  skip_if_not(is_torch_working())

  set.seed(2832)
  nnet_fit <-
    mlp(hidden_units = 2, learn_rate = 0.01, epochs = 2) %>%
    set_engine("brulee_two_layer", hidden_units_2 = 3, activation_2 = "elu") %>%
    set_mode("regression") %>%
    fit(outcome ~ ., data = num_tr)
  expect_s3_class(nnet_fit, c("_brulee_two_layer_mlp", "model_fit"))

  nnet_class_pred <- predict(nnet_fit, num_te)
  expect_true(inherits(nnet_class_pred, "data.frame"))
  expect_true(nrow(nnet_class_pred) == 2)
  expect_named(nnet_class_pred, ".pred")
})

