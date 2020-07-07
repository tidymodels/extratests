context("glmnet encodings")

library(tidymodels)
data(scat, package = "modeldata")
data(ames, package = "modeldata")
data(ad_data, package = "modeldata")

scat <- na.omit(scat)

## -----------------------------------------------------------------------------

get_glmn_coefs <- function(x, penalty = 0.01) {
  x <- coef(x, s = penalty)
  x <- as.matrix(x)
  colnames(x) <- "estimate"
  rn <- rownames(x)
  x <- tibble::as_tibble(x) %>% mutate(terms = rn, penalty = penalty)
  x <- dplyr::select(x, terms, estimate, penalty)
  if (is.list(x$estimate)) {
    x$estimate <- purrr::map(x$estimate, ~ as_tibble(as.matrix(.x), rownames = "terms"))
    x <- tidyr::unnest(x, cols = c(estimate), names_repair = "minimal")
    names(x) <- c("class", "terms", "estimate", "penalty")
  }
  x
}

## -----------------------------------------------------------------------------

lin_mod <-
  linear_reg(penalty = 0.01) %>%
  set_engine("glmnet")

logistic_mod <-
  logistic_reg(penalty = 0.01) %>%
  set_engine("glmnet")

multinom_mod <-
  multinom_reg(penalty = 0.01) %>%
  set_engine("glmnet")

## -----------------------------------------------------------------------------

test_that('linear regression parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_form_fit <-
    lin_mod %>%
    fit(log10(Sale_Price) ~ Alley + Year_Built, data = ames)

  parsnip_form_names <- get_glmn_coefs(parsnip_form_fit$fit)
  expect_true(sum(grepl("^Alley", parsnip_form_names$terms)) == 2)
})

## -----------------------------------------------------------------------------

test_that('linear regression workflows', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  wflow <-
    workflow() %>%
    add_model(lin_mod) %>%
    add_formula(log10(Sale_Price) ~ Alley + Year_Built)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = ames)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    pull_workflow_fit() %>%
    pluck("fit") %>%
    get_glmn_coefs() %>%
    pull(terms)

  expect_true(sum(grepl("^Alley", parsnip_wflow_names)) == 2)
})


## -----------------------------------------------------------------------------

test_that('logistic regression parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_form_fit <-
    logistic_mod %>%
    fit(Class ~ Genotype + IL_11, data = ad_data)

  parsnip_form_names <- get_glmn_coefs(parsnip_form_fit$fit)
  expect_true(sum(grepl("^Genotype", parsnip_form_names$terms)) == 5)
})

## -----------------------------------------------------------------------------

test_that('logistic regression workflows', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  wflow <-
    workflow() %>%
    add_model(logistic_mod) %>%
    add_formula(Class ~ Genotype + IL_11)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = ad_data)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    pull_workflow_fit() %>%
    pluck("fit") %>%
    get_glmn_coefs() %>%
    pull(terms)

  expect_true(sum(grepl("^Genotype", parsnip_wflow_names)) == 5)
})


## -----------------------------------------------------------------------------

test_that('multinomial regression parsnip models with formula interface', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  parsnip_form_fit <-
    multinom_mod %>%
    fit(Species ~ Location + Age, data = scat)

  parsnip_form_names <- unique(get_glmn_coefs(parsnip_form_fit$fit))
  expect_true(sum(grepl("^Location", unique(parsnip_form_names$terms))) == 2)
})

## -----------------------------------------------------------------------------

test_that('multinomial regression workflows', {
  skip_if(utils::packageVersion("parsnip") <= "0.1.1")

  wflow <-
    workflow() %>%
    add_model(multinom_mod) %>%
    add_formula(Species ~ Location + Age)

  parsnip_wflow_fit <-
    wflow %>%
    fit(data = scat)

  parsnip_wflow_names <-
    parsnip_wflow_fit %>%
    pull_workflow_fit() %>%
    pluck("fit") %>%
    get_glmn_coefs() %>%
    pull(terms) %>%
    unique()

  expect_true(sum(grepl("^Location", parsnip_wflow_names)) == 2)
})
