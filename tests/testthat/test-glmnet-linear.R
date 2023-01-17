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

test_that('glmnet outcome errors', {
  skip_if_not_installed("glmnet")
  skip_if(utils::packageVersion("parsnip") < "0.1.7.9003")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[1:150, c(2:5, 8)]

  hpc_basic <- linear_reg(penalty = .1, mixture = .3) %>%
    set_engine("glmnet", nlambda = 15)
  caught_ctrl <- control_parsnip(verbosity = 1, catch = TRUE)
  num_pred <- c("compounds", "iterations", "num_pending")

  expect_error(
    fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = factor(hpc$input_fields),
      control = caught_ctrl
    ),
    "For a regression model"
  )

  hpc$class <- as.character(hpc$class)

  expect_error(
    fit(
      hpc_basic,
      class ~ compounds + iterations,
      data = hpc,
      control = ctrl
    ),
    "For a regression model"
  )
})

test_that('glmnet prediction, single lambda', {

  skip_if_not_installed("glmnet")

  data("hpc_data", package = "modeldata", envir = rlang::current_env())
  hpc <- hpc_data[1:150, c(2:5, 8)]

  hpc_basic <- linear_reg(penalty = .1, mixture = .3) %>%
    set_engine("glmnet", nlambda = 15)
  ctrl <- control_parsnip(verbosity = 1, catch = FALSE)
  num_pred <- c("compounds", "iterations", "num_pending")

  res_xy <- fit_xy(
    hpc_basic,
    control = ctrl,
    x = hpc[, num_pred],
    y = hpc$input_fields
  )

  # glmn_mod <- glmnet::glmnet(x = as.matrix(hpc[, num_pred]), y = hpc$input_fields,
  #                            alpha = .3, nlambda = 15)

  uni_pred <- c(640.599944271351, 196.646976529848, 186.279646400216, 194.673852228774,
                198.126819755653)

  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred, tolerance = 0.0001)
  expect_equal(uni_pred[3], predict(res_xy, hpc[3, num_pred])$.pred, tolerance = 0.0001)
  expect_equal(
    predict(res_xy, hpc[1:5, num_pred]),
    predict(res_xy, hpc[1:5, sample(num_pred)])
  )

  res_form <- fit(
    hpc_basic,
    input_fields ~ log(compounds) + class,
    data = hpc,
    control = ctrl
  )

  form_pred <- c(570.504089227118, 162.413061474088, 167.022896537861, 157.609071878082,
                 165.887783741483)

  expect_equal(form_pred, predict(res_form, hpc[1:5,])$.pred, tolerance = 0.0001)
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

  expect_error(
    linear_reg(penalty = 0.01) %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ], penalty = 0:1)
  )
  expect_error(
    linear_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ])
  )

})

