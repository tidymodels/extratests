library(testthat)
library(recipes)


test_that('Correct values', {

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf(all_predictors(), seed = 2432, num_run = 3)

  # # make test cases
  # dat <- loadDataSet("Iris")
  # factorization <- embed(dat, "NNMF", seed = 2432, nrun = 3)
  # proj_dat <- factorization@apply(dat)
  # nn_proj <- predict(factorization, iris[1:7, 1:4])
  exp_w <-
    structure(
      c(
        6.71016847274813,
        2.16770797271576,
        7.34922929728738,
        2.74333149635337,
        7.51403209366464,
        5.31462287720424,
        1.7084186413615,
        0.135494790327036
      ),
      .Dim = c(4L, 2L),
      .Dimnames = list(
        c("Sepal.Length", "Sepal.Width",
          "Petal.Length", "Petal.Width"),
        c("NNMF1", "NNMF2")
      )
    )
  exp_pred <-
    structure(
      c(
        0.0413121365040742,
        0.0651534731267683,
        0.0414420950716912,
        0.0707413622154126,
        0.0356815850198672,
        0.0715608797697106,
        0.0488015360189133,
        0.641793359890731,
        0.573960443467727,
        0.587247370076039,
        0.551479902320343,
        0.644093699602747,
        0.67208687756876,
        0.586618960413292
      ),
      .Dim = c(7L,
               2L),
      .Dimnames = list(NULL,
                       c("NNMF1", "NNMF2"))
    )

  expect_output(print(rec))
  expect_output(rec <- prep(rec, training = iris, verbose = TRUE))

  rec_res <- juice(rec, all_predictors(), composition = "matrix")[1:7,]

  expect_equal(rec$steps[[1]]$res@other.data$w, exp_w)
  expect_equal(exp_pred, rec_res)

})


test_that('No NNF', {

  rec <- recipe(Species ~ ., data = iris) %>%
    step_nnmf(all_predictors(), seed = 2432, num_comp = 0) %>%
    prep()

  expect_equal(
    names(juice(rec)),
    names(iris)
  )
  expect_true(inherits(rec$steps[[1]]$res, "list"))
  expect_output(print(rec),
                regexp = "factorization was not done")
  expect_true(all(is.na(tidy(rec, 1)$value)))
})


test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_nnmf(all_predictors())
  rec_param <- tunable(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp", "num_run"))
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
    step_nnmf(all_predictors(), seed = 2432, num_run = 3, keep_original_cols = TRUE)

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
    step_nnmf(all_predictors(), seed = 2432, num_run = 3)

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
    step_nnmf(disp, wt, id = "test", seed = 1)
  rec_prep <- prep(rec)
  wts <- rec_prep$steps[[1]]$res@other.data$w


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
      terms = rep(c("disp", "wt"), 2),
      value = unname(c(wts[,1], wts[,2])),
      component = rep(c("NNMF1", "NNMF2"), each = 2),
      id = "test"
    )
  )
})

