library(testthat)
library(recipes)
library(Matrix) # Waiting for fix in RcppML

recipes_version <- function() paste0("recipes", packageVersion("recipes"))

test_that('Correct values', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf_sparse(all_predictors(), seed = 2432)

  dat <- Matrix::Matrix(t(as.matrix(iris[, -5])), sparse = TRUE)
  res <- RcppML::nmf(
    A = dat, k = 2, L1 = c(0.001, 0.001), verbose = FALSE,
    seed = 2432, nonneg = TRUE
  )
  exp_w <- res$w
  exp_pred <- as.matrix(iris[1:10, -5]) %*% res$w

  expect_snapshot(print(rec), variant = recipes_version())
  expect_snapshot(rec <- prep(rec, training = iris, verbose = TRUE))

  rec_res <- juice(rec, all_predictors(), composition = "matrix")[1:10,]

  expect_equal(unname(rec$steps[[1]]$res$w), exp_w)
  expect_equal(unname(exp_pred), unname(rec_res))
})

test_that('No NNF', {

  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf_sparse(all_predictors(), seed = 2432, num_comp = 0) %>%
    prep()

  expect_equal(
    names(juice(rec)),
    names(iris)
  )
  expect_equal(rec$steps[[1]]$res$w, NULL)
  expect_snapshot(print(rec), variant = recipes_version())
  expect_true(all(is.na(tidy(rec, 1)$value)))
})

test_that('tunable', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")
  rec <-
    recipe(~ ., data = iris) %>%
    step_nnmf_sparse(all_predictors())
  rec_param <- tunable(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp", "penalty"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})

test_that('keep_original_cols works', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf_sparse(all_predictors(), seed = 2432, keep_original_cols = TRUE)

  nnmf_trained <- prep(rec, training = iris, verbose = FALSE)

  nnmf_pred <- bake(nnmf_trained, new_data = iris[1:10,], all_predictors())

  expect_equal(
    colnames(nnmf_pred),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "NNMF1", "NNMF2")
  )
})

test_that('can prep recipes with no keep_original_cols', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf_sparse(all_predictors(), seed = 2432)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_warning(
    nnmf_trained <- prep(rec, training = iris, verbose = FALSE),
    "'keep_original_cols' was added to"
  )

  expect_error(
    nnmf_pred <- bake(nnmf_trained, new_data = iris, all_predictors()),
    NA
  )
})

test_that('tidy method', {
  skip_if(utils::packageVersion("recipes") < "0.1.17.9001")

  set.seed(1)
  rec <- recipe(~ ., data = mtcars) %>%
    step_nnmf_sparse(disp, wt, id = "test", seed = 1)
  rec_prep <- prep(rec)
  wts <- rec_prep$steps[[1]]$res$w


  expect_equal(
    tidy(rec, 1),
    tibble::tribble(
      ~terms, ~value, ~component,    ~id,
      "disp",   NA_real_,          2, "test",
      "wt",     NA_real_,          2, "test"
    )
  )

  expect_equal(
    tidy(rec_prep, 1),
    tibble::tibble(
      terms = setNames(rep(c("disp", "wt"), 2), rep(c("disp", "wt"), 2)),
      value = unname(c(wts[,1], wts[,2])),
      component = rep(c("NNMF1", "NNMF2"), each = 2),
      id = "test"
    )
  )
})
