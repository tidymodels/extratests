context("survivial encodings")

library(tidymodels)
library(survival)
data(diabetic)

## -----------------------------------------------------------------------------

parsnip_mod <-
  surv_reg() %>%
  set_engine("survival") %>%
  set_mode("regression")

## -----------------------------------------------------------------------------

test_that('parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_form_fit <-
    parsnip_mod %>%
    fit(Surv(time, status) ~ laser + age, data = diabetic)

  parsnip_form_names <- names(coef(parsnip_form_fit$fit))
  expect_true(sum(grepl("^laser", parsnip_form_names)) == 1)
})

## -----------------------------------------------------------------------------

test_that('workflows', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  wflow <-
    workflow() %>%
    add_model(parsnip_mod, formula = Surv(time, status) ~ laser + age) %>%
    add_formula(time + status ~ laser + age)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = diabetic)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    pull_workflow_fit() %>%
    pluck("fit") %>%
    coef() %>%
    names()

  expect_true(sum(grepl("^laser", parsnip_wflow_names)) == 1)

})



