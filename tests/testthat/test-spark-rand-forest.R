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

  suppressPackageStartupMessages(library(sparklyr))

  sc <- try(spark_connect(master = "local"), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  hpc_rf_tr <- copy_to(sc, hpc[-(1:4),   ], "hpc_rf_tr", overwrite = TRUE)
  hpc_rf_te <- copy_to(sc, hpc[  1:4 , -1], "hpc_rf_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_reg_fit <-
      fit(
        rand_forest(trees = 5, mode = "regression") %>%
          set_engine("spark", seed = 12),
        control = ctrl,
        compounds ~ .,
        data = hpc_rf_tr
      ),
    regexp = NA
  )

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_reg_fit_dup <-
      fit(
        rand_forest(trees = 5, mode = "regression") %>%
          set_engine("spark", seed = 12),
        control = ctrl,
        compounds ~ .,
        data = hpc_rf_tr
      ),
    regexp = NA
  )

  expect_error(
    spark_reg_pred <- predict(spark_reg_fit, hpc_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_pred_num <- predict(spark_reg_fit, hpc_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_dup <- predict(spark_reg_fit_dup, hpc_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_num_dup <- predict(spark_reg_fit_dup, hpc_rf_te),
    regexp = NA
  )

  expect_equal(colnames(spark_reg_pred), "pred")

  expect_equal(
    as.data.frame(spark_reg_pred)$pred,
    as.data.frame(spark_reg_dup)$pred
  )
  expect_equal(
    as.data.frame(spark_reg_pred_num)$pred,
    as.data.frame(spark_reg_num_dup)$pred
  )


  # ----------------------------------------------------------------------------

  # same for classification

  churn_rf_tr <- copy_to(sc, wa_churn[ 5:100,   ], "churn_rf_tr", overwrite = TRUE)
  churn_rf_te <- copy_to(sc, wa_churn[   1:4, -1], "churn_rf_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_class_fit <-
      fit(
        rand_forest(trees = 5, mode = "classification") %>%
          set_engine("spark", seed = 12),
        control = ctrl,
        churn ~ .,
        data = churn_rf_tr
      ),
    regexp = NA
  )

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_class_fit_dup <-
      fit(
        rand_forest(trees = 5, mode = "classification") %>%
          set_engine("spark", seed = 12),
        control = ctrl,
        churn ~ .,
        data = churn_rf_tr
      ),
    regexp = NA
  )

  expect_error(
    spark_class_pred <- predict(spark_class_fit, churn_rf_te),
    regexp = NA
  )

  expect_error(
    spark_class_pred_class <- predict(spark_class_fit, churn_rf_te),
    regexp = NA
  )

  expect_error(
    spark_class_dup <- predict(spark_class_fit_dup, churn_rf_te),
    regexp = NA
  )

  expect_error(
    spark_class_dup_class <- predict(spark_class_fit_dup, churn_rf_te),
    regexp = NA
  )

  expect_equal(colnames(spark_class_pred), "pred_class")

  expect_equal(
    as.data.frame(spark_class_pred)$pred_class,
    as.data.frame(spark_class_dup)$pred_class
  )
  expect_equal(
    as.data.frame(spark_class_pred_class)$pred_class,
    as.data.frame(spark_class_dup_class)$pred_class
  )


  expect_error(
    spark_class_prob <- predict(spark_class_fit, churn_rf_te, type = "prob"),
    regexp = NA
  )

  expect_error(
    spark_class_dup <- predict(spark_class_fit_dup, churn_rf_te, type = "prob"),
    regexp = NA
  )

  expect_error(
    spark_class_dup_classprob <- predict(spark_class_fit_dup, churn_rf_te, type = "prob"),
    regexp = NA
  )
  expect_error(
    spark_class_prob_classprob <- predict(spark_class_fit, churn_rf_te, type = "prob"),
    regexp = NA
  )

  expect_equal(colnames(spark_class_prob), c("pred_No", "pred_Yes"))

  expect_equal(
    as.data.frame(spark_class_prob),
    as.data.frame(spark_class_dup),
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(spark_class_prob_classprob),
    as.data.frame(spark_class_dup_classprob)
  )

  spark_disconnect_all()
})

