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
  skip_if_not_installed("poissonreg", minimum_version = "1.0.1.9000")
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
  skip_if_not_installed("glmnet")

  expect_snapshot(error = TRUE, {
    poisson_reg(penalty = 0.1) %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ], penalty = 0:1)
  })
  expect_error(
    poisson_reg() %>%
      set_engine("glmnet") %>%
      fit(mpg ~ ., data = mtcars[-(1:4), ]) %>%
      predict(mtcars[-(1:4), ])
  )
})
