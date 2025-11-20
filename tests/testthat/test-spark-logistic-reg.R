## Skip entire file is Spark is not installed
skip_if(spark_not_installed())
skip_if_not_installed("sparklyr", minimum_version = "1.9.1.9000")

library(testthat)
library(parsnip)
library(dplyr)

# ------------------------------------------------------------------------------

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('spark execution', {
  skip_if_not_installed("sparklyr")

  sc <- try(spark_test_connection(), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  churn_logit_tr <- copy_to(
    sc,
    wa_churn[5:100, ],
    "churn_logit_tr",
    overwrite = TRUE
  )
  churn_logit_te <- copy_to(
    sc,
    wa_churn[1:4, -1],
    "churn_logit_te",
    overwrite = TRUE
  )

  # ----------------------------------------------------------------------------

  expect_error(
    spark_class_fit <-
      fit(
        logistic_reg() %>% set_engine("spark"),
        control = ctrl,
        churn ~ .,
        data = churn_logit_tr
      ),
    regexp = NA
  )

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_class_fit_dup <-
      fit(
        logistic_reg() %>% set_engine("spark"),
        control = ctrl,
        churn ~ .,
        data = churn_logit_tr
      ),
    regexp = NA
  )

  expect_error(
    spark_class_pred <- predict(spark_class_fit, churn_logit_te),
    regexp = NA
  )

  expect_error(
    spark_class_pred_class <- predict(spark_class_fit, churn_logit_te),
    regexp = NA
  )

  expect_equal(colnames(spark_class_pred), "pred_class")

  expect_equal(
    as.data.frame(spark_class_pred)$pred_class,
    as.data.frame(spark_class_pred_class)$pred_class
  )

  expect_error(
    spark_class_prob <- predict(spark_class_fit, churn_logit_te, type = "prob"),
    regexp = NA
  )

  expect_error(
    spark_class_prob_classprob <- predict(
      spark_class_fit,
      churn_logit_te,
      type = "prob"
    ),
    regexp = NA
  )

  expect_equal(colnames(spark_class_prob), c("pred_No", "pred_Yes"))

  expect_equal(
    as.data.frame(spark_class_prob),
    as.data.frame(spark_class_prob_classprob),
    ignore_attr = TRUE
  )
})
