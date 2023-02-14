test_that("reverse Kaplan-Meier curves", {
  skip_if(utils::packageVersion("parsnip") < "1.0.3.9003")

  library(dplyr)
  library(parsnip)
  library(survival)
  library(censored)
  library(prodlim)

  # ------------------------------------------------------------------------------

  lung_dat <-
    lung %>%
    select(time, status, age, female = sex) %>%
    mutate(status = status - 1, female = female - 1)

  lung_tr <- lung_dat[1:200, ]
  lung_te <- lung_dat[201:nrow(lung_dat), ]

  test_times <- (1:10) * 100

  # ------------------------------------------------------------------------------

  weib_fit <-
    survival_reg() %>% fit(Surv(time, status) ~ age + female, data = lung_tr)

  # For testing purposes
  attr(weib_fit$censor_probs$formula, ".Environment") <- rlang::base_env()
  psnip_df <- as_tibble(weib_fit$censor_probs$fit[1:6])

  alt_obj <- weib_fit$censor_probs
  class(alt_obj) <- "censoring_model"

  prdlim <-
    prodlim::prodlim(
      Surv(time, status) ~ 1,
      data = lung_tr,
      reverse = TRUE,
      type = "surv"
    )
  prdlim_df <- as_tibble(prdlim[1:6])

  # ------------------------------------------------------------------------------

  expect_true( "censor_probs" %in% names(weib_fit) )
  expect_snapshot( print(weib_fit$censor_probs) )
  expect_equal(
    prdlim_df,
    psnip_df
  )
  expect_equal(
    names(predict(weib_fit$censor_probs, time = test_times)),
    ".prob_censored"
  )
  expect_equal(
    predict(weib_fit$censor_probs, time = test_times)$.prob_censored,
    predict(prdlim, time = test_times, type = "surv")
  )
  expect_true(
    inherits(
      predict(weib_fit$censor_probs, time = test_times, as_vector = TRUE),
      "numeric"
    )
  )
  expect_equal(
    length(predict(weib_fit$censor_probs, time = test_times, as_vector = TRUE)),
    length(test_times)
  )
  miss_pred <- predict(weib_fit$censor_probs, time = c(NA_real_, test_times), as_vector = TRUE)
  expect_equal(
    length(miss_pred),
    length(test_times) + 1
  )
  expect_equal(
    sum(is.na(miss_pred)),
    1L
  )
  expect_equal(
    which(is.na(miss_pred)),
    1
  )
  expect_equal(
    predict(weib_fit$censor_probs, time = test_times[1])$.prob_censored,
    predict(prdlim, time = test_times[1], type = "surv")
  )
  expect_snapshot_error( predict(alt_obj, time = test_times) )
  expect_equal(
    parsnip:::reverse_km(linear_reg(), NULL),
    list()
  )

})
