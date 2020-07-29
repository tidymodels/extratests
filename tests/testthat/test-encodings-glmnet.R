context("glmnet encodings")

library(tidymodels)
data(ames, package = "modeldata")

## -----------------------------------------------------------------------------

parsnip_mod <-
  linear_reg(penalty = .1) %>%
  set_engine("glmnet")

## -----------------------------------------------------------------------------

test_that('parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.2")

  parsnip_form_fit <-
    parsnip_mod %>%
    fit(Sale_Price ~ Year_Built + Alley, data = ames)

  parsnip_form_names <- tidy(parsnip_form_fit)$term

  expect_true(sum(grepl("(Intercept)", parsnip_form_names)) == 1)
  expect_true(sum(grepl("^Alley", parsnip_form_names)) == 2)
})

test_that('parsnip models with xy interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.2")

  expect_warning(
    expect_error(
      parsnip_mod %>%
        fit_xy(x = ames[, c("Year_Built", "Alley")], y = ames$Sale_Price)
    )
  )

  parsnip_xy_fit <-
    parsnip_mod %>%
    fit_xy(x = ames[, c("Year_Built", "Longitude")], y = ames$Sale_Price)

  parsnip_xy_names <- tidy(parsnip_xy_fit)$term

  expect_true(sum(grepl("(Intercept)", parsnip_xy_names)) == 1)
})

## -----------------------------------------------------------------------------

test_that('workflows', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.2")

  wflow <-
    workflow() %>%
    add_model(parsnip_mod) %>%
    add_formula(Sale_Price ~ Year_Built + Alley)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = ames)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    pull_workflow_fit() %>%
    tidy() %>%
    pull(term)

  expect_true(sum(grepl("(Intercept)", parsnip_wflow_names)) == 1)
  expect_true(sum(grepl("^Alley", parsnip_wflow_names)) == 2)
})



