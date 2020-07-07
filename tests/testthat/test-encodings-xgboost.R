context("xgboost encodings")

library(tidymodels)
data(scat, package = "modeldata")
scat <- na.omit(scat)

## -----------------------------------------------------------------------------

parsnip_mod <-
  boost_tree(trees = 20) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## -----------------------------------------------------------------------------

test_that('parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_form_fit <-
    parsnip_mod %>%
    fit(Species ~ ., data = scat)

  parsnip_form_names <- parsnip_form_fit$fit$feature_names
  expect_true(sum(grepl("Location", parsnip_form_names)) == length(levels(scat$Location)))
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
    pluck("feature_names")

  expect_true(sum(grepl("Location", parsnip_wflow_names)) == length(levels(scat$Location)))

})
