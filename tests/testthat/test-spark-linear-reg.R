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

  hpc_linreg_tr <- copy_to(sc, hpc[-(1:4),   ], "hpc_linreg_tr", overwrite = TRUE)
  hpc_linreg_te <- copy_to(sc, hpc[  1:4 , -1], "hpc_linreg_te", overwrite = TRUE)

  expect_error(
    spark_fit <-
      fit(
        linear_reg() %>% set_engine("spark"),
        control = ctrl,
        compounds ~ .,
        data = hpc_linreg_tr
      ),
    regexp = NA
  )

  expect_false(has_multi_predict(spark_fit))
  expect_equal(multi_predict_args(spark_fit), NA_character_)

  expect_error(
    spark_pred <- predict(spark_fit, hpc_linreg_te),
    regexp = NA
  )

  expect_error(
    spark_pred_num <- predict(spark_fit, hpc_linreg_te),
    regexp = NA
  )

  lm_fit <- lm(compounds ~ ., data = hpc[-(1:4),   ])
  lm_pred <- unname(predict(lm_fit, hpc[  1:4 , -1]))

  expect_equal(as.data.frame(spark_pred)$pred, lm_pred)
  expect_equal(as.data.frame(spark_pred_num)$pred, lm_pred)

  spark_disconnect_all()
})

