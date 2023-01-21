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
