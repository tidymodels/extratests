library(poissonreg)

test_that("glmnet model object", {
  skip_if_not_installed("glmnet")

  data(seniors, package = "poissonreg", envir = rlang::current_env())
  seniors_x <- model.matrix(~ ., data = seniors[, -4])[, -1]
  seniors_y <- seniors$count

  exp_fit <- glmnet::glmnet(x = seniors_x, y = seniors_y, family = "poisson",
                            alpha = 0.3, nlambda = 15)

  spec <- poisson_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)

  expect_no_error({
    f_fit <- fit(spec, count ~ ., data = seniors)
  })
  expect_no_error({
    xy_fit <- fit_xy(spec, x = seniors_x, y = seniors_y)
  })

  expect_equal(f_fit$fit, xy_fit$fit)
  # removing call element
  expect_equal(f_fit$fit[-11], exp_fit[-11])
})

test_that('glmnet prediction, single lambda', {
  skip_on_cran()
  skip_if_not_installed("glmnet")

  data(seniors, package = "poissonreg", envir = rlang::current_env())
  senior_ind <- model.matrix(~ ., data = seniors)[, -1]
  senior_ind <- tibble::as_tibble(senior_ind)

  glmn_spec <- poisson_reg(penalty = .01, mixture = .3) %>% set_engine("glmnet")
  ctrl <- control_parsnip(verbosity = 1, catch = FALSE)

  res_xy <- fit_xy(
    glmn_spec,
    control = ctrl,
    x = senior_ind[, 1:3],
    y = senior_ind$count
  )

  uni_pred <- c(538.72595867201, 735.342850038907, 283.116497320853, 386.444515401008,
                92.1247665366651)

  expect_equal(uni_pred, predict(res_xy, senior_ind[1:5, 1:3])$.pred, tolerance = 0.0001)

  res_form <- fit(
    glmn_spec,
    count ~ .,
    data = seniors,
    control = ctrl
  )

  expect_equal(uni_pred, predict(res_form, seniors[1:5, 1:3])$.pred, tolerance = 0.0001)
})

test_that("glmnet prediction: type numeric", {
  skip_if_not_installed("glmnet")

  data(seniors, package = "poissonreg", envir = rlang::current_env())
  seniors_x <- model.matrix(~ ., data = seniors[, -4])[, -1]
  seniors_y <- seniors$count

  exp_fit <- glmnet::glmnet(x = seniors_x, y = seniors_y, family = "poisson",
                            alpha = 0.3, nlambda = 15)
  exp_pred <- predict(exp_fit, seniors_x, s = 0.1, type = "response")

  spec <- poisson_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  f_fit <- fit(spec, count ~ ., data = seniors)
  xy_fit <- fit_xy(spec, x = seniors_x, y = seniors_y)

  f_pred <- predict(f_fit, seniors)
  xy_pred <- predict(xy_fit, seniors_x)
  expect_equal(f_pred, xy_pred)
  expect_equal(f_pred$.pred, as.vector(exp_pred))

  # check format
  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(seniors))

  # single prediction
  f_pred_1 <- predict(f_fit, seniors[1, ])
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, seniors_x[1, , drop = FALSE])
  expect_equal(nrow(xy_pred_1), 1)
})

test_that('glmnet prediction: column order of `new_data` irrelevant', {
  skip_if_not_installed("glmnet")

  data(seniors, package = "poissonreg", envir = rlang::current_env())
  seniors_x <- model.matrix(~ ., data = seniors[, -4])[, -1]
  seniors_y <- seniors$count

  spec <- poisson_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  xy_fit <- fit_xy(spec, x = seniors_x, y = seniors_y)

  expect_equal(
    predict(xy_fit, seniors_x[, 3:1]),
    predict(xy_fit, seniors_x)
  )
})

test_that("glmnet prediction: type raw", {
  skip_if_not_installed("glmnet")

  data(seniors, package = "poissonreg", envir = rlang::current_env())
  seniors_x <- model.matrix(~ ., data = seniors[, -4])[, -1]
  seniors_y <- seniors$count

  exp_fit <- glmnet::glmnet(x = seniors_x, y = seniors_y, family = "poisson",
                            alpha = 0.3, nlambda = 15)
  exp_pred <- predict(exp_fit, seniors_x, s = 0.1)

  spec <- poisson_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  f_fit <- fit(spec, count ~ ., data = seniors)
  xy_fit <- fit_xy(spec, x = seniors_x, y = seniors_y)

  f_pred <- predict(f_fit, seniors, type = "raw")
  xy_pred <- predict(xy_fit, seniors_x, type = "raw")
  expect_equal(f_pred, xy_pred)
  expect_equal(f_pred, exp_pred)

  # single prediction
  f_pred_1 <- predict(f_fit, seniors[1, ], type = "raw")
  expect_equal(nrow(f_pred_1), 1)
  xy_pred_1 <- predict(xy_fit, seniors_x[1, , drop = FALSE], type = "raw")
  expect_equal(nrow(xy_pred_1), 1)
})

test_that("formula interface can deal with missing values", {
  skip_if_not_installed("glmnet")

  data(seniors, package = "poissonreg", envir = rlang::current_env())

  seniors$alcohol[1] <- NA

  spec <- poisson_reg(penalty = 0.1, mixture = 0.3) %>%
    set_engine("glmnet", nlambda = 15)
  f_fit <- fit(spec, count ~ ., data = seniors)

  f_pred <- predict(f_fit, seniors)
  expect_equal(nrow(f_pred), nrow(seniors))
  expect_true(is.na(f_pred$.pred[1]))
})

test_that('error traps', {
  skip_on_cran()
  skip_if_not_installed("glmnet")

  expect_error(
    poisson_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ], penalty = 0:1)
  )
  expect_error(
    poisson_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ])
  )
})
