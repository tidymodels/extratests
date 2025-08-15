skip_if_not_installed("parsnip", minimum_version = "1.1.1.9004")

test_that("preprocessing formula doesn't result in extra intercept - xy_xy()", {
  wflow_fit <- workflow() %>%
    add_formula(mpg ~ cyl + disp + hp) %>%
    add_model(linear_reg(penalty = 0.1, engine = "glmnet")) %>%
    fit(data = mtcars)

  coef_names <- wflow_fit %>%
    extract_fit_engine() %>%
    coef() %>%
    rownames()
  expect_equal(coef_names, c("(Intercept)", "cyl", "disp", "hp"))

  expect_no_error(
    predict(wflow_fit, new_data = mtcars)
  )
})

test_that("preprocessing formula doesn't result in extra intercept - xy_form()", {
  wflow_fit <- workflow() %>%
    add_formula(mpg ~ cyl + disp + hp) %>%
    add_model(linear_reg(engine = "lm")) %>%
    fit(data = mtcars)

  coef_names <- wflow_fit %>%
    extract_fit_engine() %>%
    coef() %>%
    names()
  expect_equal(coef_names, c("(Intercept)", "cyl", "disp", "hp"))

  expect_no_error(
    predict(wflow_fit, new_data = mtcars)
  )
})

test_that("preprocessing formula doesn't result in extra intercept - form_xy()", {
  wflow_fit <- workflow() %>%
    add_formula(mpg ~ cyl + disp + hp) %>%
    add_model(
      linear_reg(penalty = 0.1, engine = "glmnet"),
      formula = mpg ~ .
    ) %>%
    fit(data = mtcars)

  coef_names <- wflow_fit %>%
    extract_fit_engine() %>%
    coef() %>%
    rownames()
  expect_equal(coef_names, c("(Intercept)", "cyl", "disp", "hp"))

  expect_no_error(
    predict(wflow_fit, new_data = mtcars)
  )
})

test_that("preprocessing formula doesn't result in extra intercept - form_form()", {
  wflow_fit <- workflow() %>%
    add_formula(mpg ~ cyl + disp + hp) %>%
    add_model(linear_reg(engine = "lm"), formula = mpg ~ .) %>%
    fit(data = mtcars)

  coef_names <- wflow_fit %>%
    extract_fit_engine() %>%
    coef() %>%
    names()
  expect_equal(coef_names, c("(Intercept)", "cyl", "disp", "hp"))

  expect_no_error(
    predict(wflow_fit, new_data = mtcars)
  )
})
