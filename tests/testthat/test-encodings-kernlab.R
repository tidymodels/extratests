context("kernlab encodings")

library(tidymodels)
data(scat, package = "modeldata")
scat <- na.omit(scat)

## -----------------------------------------------------------------------------

parsnip_mod <-
  svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("regression")

## -----------------------------------------------------------------------------

test_that('parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_form_fit <-
    parsnip_mod %>%
    fit(Species ~ Location + Age, data = scat)

  parsnip_form_names <- colnames(parsnip_form_fit$fit@xmatrix[[1]])
  expect_true(sum(grepl("Location", parsnip_form_names)) == 3)
})

test_that('parsnip models with xy interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_xy_fit <-
    parsnip_mod %>%
    fit_xy(x = scat[, c("Location", "Age")], y = scat$Species)

  parsnip_form_names <- colnames(parsnip_xy_fit$fit@xmatrix[[1]])
  expect_true(sum(grepl("Location", parsnip_form_names)) == 3)

})

## -----------------------------------------------------------------------------

test_that('workflows', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  wflow <-
    workflow() %>%
    add_model(parsnip_mod) %>%
    add_formula(Species ~ Location + Age)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = scat)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    pull_workflow_fit() %>%
    pluck("fit")

  parsnip_wflow_names <- parsnip_wflow_names@xmatrix[[1]] %>% colnames()

  expect_true(sum(grepl("Location", parsnip_wflow_names)) == 3)

})



