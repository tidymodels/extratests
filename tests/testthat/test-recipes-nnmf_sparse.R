library(testthat)
library(recipes)
library(Matrix) # Waiting for fix in RcppML

test_that('Correct values', {

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf_sparse(all_predictors(), seed = 2432)

  # dat <- Matrix::Matrix(t(as.matrix(iris[, -5])), sparse = TRUE)
  # res <- RcppML::nmf(
  #   A = dat, k = 2, L1 = c(0.001, 0.001), verbose = FALSE,
  #   seed = 2432, nonneg = TRUE
  # )
  # dput(res$w)
  # dput(as.matrix(iris[1:10, -5]) %*% res$w)

  exp_w <-
    structure(c(
      0.503640687085334, 0.341172873400033, 0.136435348037937,
      0.0187510914766956, 0.270387203970495, 0, 0.518315790606461,
      0.211297005423044
    ), .Dim = c(4L, 2L))
  exp_pred <-
    structure(c(
      3.95743226658377, 3.68611769246669, 3.63998059492583,
      3.58278630848488, 3.94118548521524, 4.2896744447761, 3.67336974484877,
      3.88659444533903, 3.40018006158402, 3.73200340546282, 2.14687624818318,
      2.09279880738908, 1.98688978753433, 2.06351422525858, 2.11983752778613,
      2.42574654764087, 2.03281234674023, 2.17166910684677, 1.95760520540383,
      2.12350068590742
    ), .Dim = c(10L, 2L), .Dimnames = list(c(
      "1",
      "2", "3", "4", "5", "6", "7", "8", "9", "10"
    ), NULL))

  expect_output(print(rec))
  expect_output(rec <- prep(rec, training = iris, verbose = TRUE))

  rec_res <- juice(rec, all_predictors(), composition = "matrix")[1:10,]

  expect_equal(unname(rec$steps[[1]]$res$w), exp_w)
  expect_equal(unname(exp_pred), unname(rec_res))
})

test_that('No NNF', {

  skip_if(utils::packageVersion("recipes") < "0.1.17.9000")

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf_sparse(all_predictors(), seed = 2432, num_comp = 0) %>%
    prep()

  expect_equal(
    names(juice(rec)),
    names(iris)
  )
  expect_equal(rec$steps[[1]]$res$w, NULL)
  expect_output(print(rec),
                regexp = "No non-negative matrix factorization")
  expect_true(all(is.na(tidy(rec, 1)$value)))
})

test_that('tunable', {
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
