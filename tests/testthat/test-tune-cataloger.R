test_that("issue cataloger works (finetune)", {
  skip_if(packageVersion("tune") < "1.0.1.9002")
  skip_if(packageVersion("finetune") < "1.0.1.9002")

  library(tune)
  library(finetune)
  library(modeldata)
  library(parsnip)
  library(rsample)
  library(mockery)
  library(yardstick)

  stub(tune:::log_problems, "tune:::is_testing", FALSE)
  stub(tune::initialize_catalog, "tune:::is_testing", FALSE)
  stub(tune:::uses_catalog, "tune:::is_testing", FALSE)

  skip_if(tune:::allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")

  # data setup
  ames_narrow <-ames[, c(72, 40:45)]
  spec_dt_tune <- decision_tree(
    cost_complexity = tune(), min_n = tune(), mode = "regression"
  )
  form <- Sale_Price ~ .
  set.seed(1)
  folds <- vfold_cv(ames_narrow, 10)

  raise_issues <- function(x) {warning("nooo"); stop("AHHH")}

  # only works when run following `local_reproducible_output()`, as in
  # `devtools::test()`. for each test case, ensure that each issue is allotted
  # a unique and minimal number that's counted correctly in the final summary.
  res_1 <-
    capture_messages({
      set.seed(121)
      res_anova <-
        tune_race_anova(
          spec_dt_tune,
          form,
          resamples = folds,
          control = control_race(verbose_elim = TRUE, extract = raise_issues)
        )
    })

  expect_match(res_1, "1.*warning.*nooo", all = FALSE)
  expect_match(res_1, "2.*error.*AHHH", all = FALSE)
  expect_match(res_1, "with some computations.*1.*x51.*2.*x51", all = FALSE)

  res_2 <-
    capture_messages({
      set.seed(354)
      res_sa <-
        tune_sim_anneal(
          spec_dt_tune,
          form,
          resamples = folds,
          initial = res_anova,
          metrics = metric_set(rmse),
          iter = 15,
          control = control_sim_anneal(extract = raise_issues)
        )
    })

  expect_match(res_2, "1.*warning.*nooo", all = FALSE)
  expect_match(res_2, "2.*error.*AHHH", all = FALSE)
  expect_match(res_2, "with some computations.*1.*x150.*2.*x149", all = FALSE)
})
