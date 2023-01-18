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

test_that('glmnet prediction, multiple lambda', {

  skip_if_not_installed("glmnet")

  lams <- c(.01, 0.1)

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[1:150, c(2:5, 8)]

  hpc_mult <- linear_reg(penalty = 0.1, mixture = .3) %>%
    set_engine("glmnet")
  ctrl <- control_parsnip(verbosity = 1, catch = FALSE)
  num_pred <- c("compounds", "iterations", "num_pending")

  res_xy <- fit_xy(
    hpc_mult,
    control = ctrl,
    x = hpc[, num_pred],
    y = hpc$input_fields
  )

  # mult_pred <-
  #   predict(res_xy$fit,
  #           newx = as.matrix(hpc[1:5, num_pred]),
  #           s = lams)
  # mult_pred <- stack(as.data.frame(mult_pred))
  # mult_pred$penalty <- rep(lams, each = 5)
  # mult_pred$rows <- rep(1:5, 2)
  # mult_pred <- mult_pred[order(mult_pred$rows, mult_pred$penalty), ]
  # mult_pred <- mult_pred[, c("penalty", "values")]
  # names(mult_pred) <- c("penalty", ".pred")
  # mult_pred <- tibble::as_tibble(mult_pred)
  mult_pred <-
    tibble::tribble(
      ~penalty,           ~.pred,
      0.01, 639.672880668187,
      0.1, 639.672880668187,
      0.01, 197.744613311359,
      0.1, 197.744613311359,
      0.01, 187.737940787615,
      0.1, 187.737940787615,
      0.01, 195.780487678662,
      0.1, 195.780487678662,
      0.01, 199.217707535882,
      0.1, 199.217707535882
    )

  expect_equal(
    as.data.frame(mult_pred),
    multi_predict(res_xy, new_data = hpc[1:5, num_pred], penalty = lams) %>%
      unnest(cols = c(.pred)) %>%
      as.data.frame(),
    tolerance = 0.0001
  )

  res_form <- fit(
    hpc_mult,
    input_fields ~ log(compounds) + class,
    data = hpc,
    control = ctrl
  )

  # form_mat <- model.matrix(input_fields ~ log(compounds) + class, data = hpc)
  # form_mat <- form_mat[1:5, -1]
  #
  # form_pred <-
  #   predict(res_form$fit,
  #           newx = form_mat,
  #           s = lams)
  # form_pred <- stack(as.data.frame(form_pred))
  # form_pred$penalty <- rep(lams, each = 5)
  # form_pred$rows <- rep(1:5, 2)
  # form_pred <- form_pred[order(form_pred$rows, form_pred$penalty), ]
  # form_pred <- form_pred[, c("penalty", "values")]
  # names(form_pred) <- c("penalty", ".pred")
  # form_pred <- tibble::as_tibble(form_pred)

  form_pred <-
    tibble::tribble(
      ~penalty,           ~.pred,
      0.01, 570.474473760044,
      0.1, 570.474473760044,
      0.01, 164.040104978709,
      0.1, 164.040104978709,
      0.01, 168.709676954287,
      0.1, 168.709676954287,
      0.01, 159.173862504055,
      0.1, 159.173862504055,
      0.01, 167.559854709074,
      0.1, 167.559854709074
    )

  expect_equal(
    as.data.frame(form_pred),
    multi_predict(res_form, new_data = hpc[1:5, ], penalty = lams) %>%
      unnest(cols = c(.pred)) %>%
      as.data.frame(),
    tolerance = 0.0001
  )
})


test_that('submodel prediction', {

  skip_if_not_installed("glmnet")

  reg_fit <-
    linear_reg(penalty = 0.1) %>%
    set_engine("glmnet") %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  pred_glmn <- predict(reg_fit$fit, as.matrix(mtcars[1:4, -1]), s = .1)

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], penalty = .1)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], unname(pred_glmn[,1]))

  expect_error(
    multi_predict(reg_fit, newdata = mtcars[1:4, -1], penalty = .1),
    "Did you mean"
  )

  reg_fit <-
    linear_reg(penalty = 0.01) %>%
    set_engine("glmnet") %>%
    fit(mpg ~ ., data = mtcars[-(1:4), ])


  pred_glmn_all <-
    predict(reg_fit$fit, as.matrix(mtcars[1:2, -1]), penalty = reg_fit$fit$lambda) %>%
    as.data.frame() %>%
    stack() %>%
    dplyr::arrange(ind)


  mp_res_all <-
    multi_predict(reg_fit, new_data = mtcars[1:2, -1], penalty = reg_fit$fit$lambda) %>%
    tidyr::unnest(cols = c(.pred))

  expect_equal(sort(mp_res_all$.pred), sort(pred_glmn_all$values))

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
