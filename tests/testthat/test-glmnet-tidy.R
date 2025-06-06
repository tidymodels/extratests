
test_that('linear regression', {
  skip_if_not_installed("glmnet")
  suppressPackageStartupMessages(library(glmnet))

  ps_mod <-
    linear_reg(penalty = .1) %>%
    set_engine("glmnet") %>%
    fit(mpg ~ ., data = mtcars)

  ps_coefs <- tidy(ps_mod)
  gn_coefs <- as.matrix(coef(ps_mod$fit, s = .1))
  for(i in ps_coefs$term) {
    expect_equal(ps_coefs$estimate[ps_coefs$term == i], gn_coefs[i,1])
  }
})

test_that('logistic regression', {
  skip_if_not_installed("glmnet")

  data(two_class_dat, package = "modeldata")

  ps_mod <-
    logistic_reg(penalty = .1) %>%
    set_engine("glmnet") %>%
    fit(Class ~ ., data = two_class_dat)

  ps_coefs <- tidy(ps_mod)
  gn_coefs <- as.matrix(coef(ps_mod$fit, s = .1))
  for(i in ps_coefs$term) {
    expect_equal(ps_coefs$estimate[ps_coefs$term == i], gn_coefs[i,1])
  }
})

test_that('multinomial regression', {
  skip_if_not_installed("glmnet")

  data(penguins, package = "modeldata")

  ps_mod <-
    multinom_reg(penalty = .01) %>%
    set_engine("glmnet") %>%
    fit(species ~ ., data = penguins)

  ps_coefs <- tidy(ps_mod)
  gn_coefs <- coef(ps_mod$fit, s = .01)
  gn_coefs <- purrr::map(gn_coefs, as.matrix)
  for(i in unique(ps_coefs$term)) {
    for(j in unique(ps_coefs$class)) {
      expect_equal(
        ps_coefs$estimate[ps_coefs$term == i & ps_coefs$class == j],
        gn_coefs[[j]][i,1]
        )
    }
  }
})

test_that('check proper penalty range', {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("parsnip", minimum_version = "1.3.0.9000")

  data(Chicago, package = "modeldata")

  Chicago <- Chicago %>% select(ridership, Clark_Lake, Austin, Harlem)

  glmnet_spec <-
    linear_reg(penalty = 0.1, mixture = 0.95) %>%
    set_engine("glmnet")

  glmnet_wflow <-
    workflow() %>%
    add_model(glmnet_spec) %>%
    add_formula(ridership ~ .)

  glmnet_fit <- fit(glmnet_wflow, Chicago)

  res <- try(tidy(glmnet_fit, penalty = 5.5620), silent = TRUE)
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))

})

