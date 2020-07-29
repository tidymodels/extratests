context("randomForest encodings")

library(tidymodels)
data(scat, package = "modeldata")
scat <- na.omit(scat)

## -----------------------------------------------------------------------------

parsnip_mod <-
  rand_forest(mtry = 4, trees = 20) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")

## -----------------------------------------------------------------------------

test_that('parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_form_fit <-
    parsnip_mod %>%
    fit(Species ~ ., data = scat)

  parsnip_form_names <- rownames(parsnip_form_fit$fit$importance)
  expect_true(sum(grepl("Location", parsnip_form_names)) == 1)
})

test_that('parsnip models with xy interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_xy_fit <-
    parsnip_mod %>%
    fit_xy(x = scat[, -1], y = scat$Species)

  parsnip_xy_names <- rownames(parsnip_xy_fit$fit$importance)
  expect_true(sum(grepl("Location", parsnip_xy_names)) == 1)

})

## -----------------------------------------------------------------------------

test_that('workflows', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  wflow <-
    workflow() %>%
    add_model(parsnip_mod) %>%
    add_formula(Species ~ .)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = scat)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    pull_workflow_fit() %>%
    pluck("fit") %>%
    pluck("importance") %>%
    rownames()

  expect_true(sum(grepl("Location", parsnip_wflow_names)) == 1)

})



