library(testthat)
library(parsnip)

R_version_too_small_for_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0
skip_if(R_version_too_small_for_glmnet)

test_that('glmnet execution error', {
  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[1:150, c(2:5, 8)]

  hpc_basic <- linear_reg(penalty = .1, mixture = .3) %>%
    set_engine("glmnet", nlambda = 15)

  # this error/test is not glmnet-specific,
  # the error comes from `parsnip::.convert_form_to_xy_fit()`
  hpc_bad_form <- as.formula(class ~ nonexistent_variable)
  expect_error(
    fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc
    )
  )
})

test_that("glmnet model object", {
  hpc <- hpc_data[1:150, c(2:5, 8)]
  hpc_x <- model.matrix(~ log(compounds) + class, data = hpc)[, -1]
  hpc_y <- hpc$input_fields

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "gaussian",
                            alpha = 0.3, nlambda = 15)

  lm_spec <- linear_reg(penalty = 0.123, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  expect_no_error(
    f_fit <- fit(lm_spec, input_fields ~ log(compounds) + class, data = hpc)
  )
  expect_no_error(
    xy_fit <- fit_xy(lm_spec, x = hpc_x, y = hpc_y)
  )

  expect_equal(f_fit$fit, xy_fit$fit)
  # removing call element
  expect_equal(f_fit$fit[-11], exp_fit[-11])
})

test_that("glmnet prediction: type numeric", {
  skip_if_not_installed("glmnet")

  hpc <- hpc_data[1:150, c(2:5, 8)]
  hpc_x <- model.matrix(~ log(compounds) + class, data = hpc)[, -1]
  hpc_y <- hpc$input_fields

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "gaussian",
                            alpha = 0.3, nlambda = 15)
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1)

  lm_spec <- linear_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  f_fit <- fit(lm_spec, input_fields ~ log(compounds) + class, data = hpc)
  xy_fit <- fit_xy(lm_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc)
  xy_pred <- predict(xy_fit, hpc_x)
  expect_equal(f_pred, xy_pred)
  expect_equal(f_pred$.pred, as.vector(exp_pred))

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(hpc))

  # single prediction
  f_pred_1 <- predict(f_fit, hpc[1, ])
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE])
  expect_equal(nrow(xy_pred_1), 1)
})

test_that('glmnet prediction: column order of `new_data` irrelevant', {

  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[1:150, c(2:5, 8)]

  hpc_basic <- linear_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  num_pred <- c("compounds", "iterations", "num_pending")

  res_xy <- fit_xy(hpc_basic, x = hpc[, num_pred], y = hpc$input_fields)

  expect_equal(
    predict(res_xy, hpc[1:5, sample(num_pred)]),
    predict(res_xy, hpc[1:5, num_pred])
  )
})

test_that("glmnet prediction: type raw", {
  skip_if_not_installed("glmnet")

  hpc <- hpc_data[1:150, c(2:5, 8)]
  hpc_x <- model.matrix(~ log(compounds) + class, data = hpc)[, -1]
  hpc_y <- hpc$input_fields

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "gaussian",
                            alpha = 0.3, nlambda = 15)
  exp_pred <- predict(exp_fit, hpc_x, s = 0.1)

  lm_spec <-  linear_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  f_fit <- fit(lm_spec, input_fields ~ log(compounds) + class, data = hpc)
  xy_fit <- fit_xy(lm_spec, x = hpc_x, y = hpc_y)

  f_pred <- predict(f_fit, hpc, type = "raw")
  xy_pred <- predict(xy_fit, hpc_x, type = "raw")
  expect_equal(f_pred, xy_pred)
  expect_equal(f_pred, exp_pred)

  # single prediction
  f_pred_1 <- predict(f_fit, hpc[1, ], type = "raw")
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, hpc_x[1, , drop = FALSE], type = "raw")
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("formula interface can deal with missing values", {
  skip_if_not_installed("glmnet")

  hpc <- hpc_data[1:150, c(2:5, 8)]

  hpc$compounds[1] <- NA
  hpc_x[1,1] <- NA

  lm_spec <- linear_reg(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(lm_spec, input_fields ~ log(compounds) + class, data = hpc)

  f_pred <- predict(f_fit, hpc)
  expect_equal(nrow(f_pred), nrow(hpc))
  expect_true(is.na(f_pred$.pred[1]))
})

test_that("glmnet multi_predict(): type numeric", {
  skip_if_not_installed("glmnet")

  hpc <- hpc_data[1:150, c(2:5, 8)]
  hpc_x <- model.matrix(~ log(compounds) + class, data = hpc)[, -1]
  hpc_y <- hpc$input_fields

  penalty_values <- c(0.01, 0.1)

  exp_fit <- glmnet::glmnet(x = hpc_x, y = hpc_y, family = "gaussian", alpha = 0.3)
  exp_pred <- predict(exp_fit, hpc_x, s = penalty_values)

  lm_spec <- linear_reg(penalty = 0.123, mixture = 0.3) %>% set_engine("glmnet")
  f_fit <- fit(lm_spec, input_fields ~ log(compounds) + class, data = hpc)
  xy_fit <- fit_xy(lm_spec, x = hpc_x, y = hpc_y)

  expect_true(has_multi_predict(xy_fit))
  expect_equal(multi_predict_args(xy_fit), "penalty")

  f_pred <- multi_predict(f_fit, hpc, penalty = penalty_values)
  xy_pred <- multi_predict(xy_fit, hpc_x, penalty = penalty_values)
  expect_equal(f_pred, xy_pred)

  f_pred_001 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.01) %>%
    dplyr::pull(.pred)
  f_pred_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::filter(penalty == 0.1) %>%
    dplyr::pull(.pred)
  expect_equal(f_pred_001, unname(exp_pred[,1]))
  expect_equal(f_pred_01, unname(exp_pred[,2]))

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(hpc))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(names(.x) == c("penalty", ".pred"))))
  )

  # single prediction
  f_pred_1 <- multi_predict(f_fit, hpc[1, ], penalty = penalty_values)
  xy_pred_1 <- multi_predict(xy_fit, hpc_x[1, , drop = FALSE], penalty = penalty_values)
  expect_equal(f_pred_1, xy_pred_1)
  expect_equal(nrow(f_pred_1), 1)
  expect_equal(nrow(f_pred_1$.pred[[1]]), 2)
})

test_that('error traps', {
  skip_if_not_installed("glmnet")

  expect_snapshot(error = TRUE, {
    linear_reg(penalty = 0.01) %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ], penalty = 0:1)
  })
  expect_snapshot(error = TRUE, {
    linear_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ])
  })
})
