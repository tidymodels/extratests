library(tidymodels)
data(scat, package = "modeldata")
scat <- na.omit(scat)

## -----------------------------------------------------------------------------

parsnip_mod <-
  rand_forest(mtry = 4, trees = 20) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

## -----------------------------------------------------------------------------

test_that('parsnip models with formula interface', {
  parsnip_form_fit <-
    parsnip_mod %>%
    fit(Species ~ ., data = scat)

  parsnip_form_names <- names(parsnip_form_fit$fit$variable.importance)
  expect_identical(sum(grepl("Location", parsnip_form_names)), 1L)

  expect_no_error(
    predict(parsnip_form_fit, scat)
  )
})

test_that('parsnip models with xy interface', {
  parsnip_xy_fit <-
    parsnip_mod %>%
    fit_xy(x = scat[, -1], y = scat$Species)

  parsnip_xy_names <- names(parsnip_xy_fit$fit$variable.importance)
  expect_identical(sum(grepl("Location", parsnip_xy_names)), 1L)

  expect_no_error(
    predict(parsnip_xy_fit, scat[, -1])
  )
})

## -----------------------------------------------------------------------------

test_that('workflows', {
  wflow <-
    workflow() %>%
    add_model(parsnip_mod) %>%
    add_formula(Species ~ .)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = scat)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    extract_fit_parsnip() %>%
    pluck("fit") %>%
    pluck("variable.importance") %>%
    names()

  expect_identical(sum(grepl("Location", parsnip_wflow_names)), 1L)

  expect_no_error(
    predict(parsnip_wflow_fit, scat[, -1])
  )
})
