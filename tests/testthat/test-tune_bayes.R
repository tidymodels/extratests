library(tidymodels)

test_that('tune recipe and model, which has_unknowns', {

  # This test needs a tuning parameter object with default 'unknown' parameter
  # values (e.g., mtry).

  skip_if_not_installed("randomForest")

  rec_tune_1 <-
    recipe(mpg ~ ., data = mtcars) %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors(), num_comp = tune())
  rf_mod <-
    rand_forest(mode = "regression", mtry = tune()) %>%
    set_engine("randomForest")
  iter1 <- 2
  iter2 <- 2
  iterT <- iter1 + iter2

  set.seed(4400)
  wflow <- workflow() %>% add_recipe(rec_tune_1) %>% add_model(rf_mod)
  pset <- extract_parameter_set_dials(wflow) %>%
    update(num_comp = num_comp(c(3, 5)),
           mtry = mtry(c(1, 3)))
  expect_true(
    any(
      vapply(
        extract_parameter_set_dials(wflow)$object,
        dials::has_unknowns,
        FUN.VALUE = TRUE
      )
    )
  )
  folds <- vfold_cv(mtcars)
  res <- tune_bayes(wflow, resamples = folds, param_info = pset,
                    initial = iter1, iter = iter2) %>%
    suppressMessages()
  expect_equal(unique(res$id), folds$id)
  expect_equal(
    colnames(res$.metrics[[1]]),
    c("mtry", "num_comp", ".metric", ".estimator", ".estimate", ".config")
  )
  res_est <- collect_metrics(res)
  expect_equal(nrow(res_est), iterT * 2)
  expect_equal(sum(res_est$.metric == "rmse"), iterT)
  expect_equal(sum(res_est$.metric == "rsq"), iterT)
  expect_equal(dplyr::n_distinct(res_est$.config), iterT)
  expect_equal(res_est$n, rep(10, iterT * 2))
})
