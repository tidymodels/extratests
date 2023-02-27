test_that('survival helpers', {
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9001")
  library(survival)
  library(parsnip)

  times <- seq(1, 100, length.out = 5)
  times2 <- seq(100, 200, length.out = 5)
  events <- c(1, 0, 1, 0, 1)

  right_c <- Surv(times, events)
  left_c <- Surv(times, events, type = "left")
  intv_c <- Surv(times, times2, events, type = "interval")
  count_c <- Surv(times, times2, events)

  expect_true(parsnip:::.is_surv(right_c))
  expect_false(parsnip:::.is_surv(1, fail = FALSE))
  expect_snapshot_error(parsnip:::.is_surv(1))

  expect_true(.is_censored_right(right_c))
  expect_false(.is_censored_right(left_c))

  expect_equal(
    .extract_surv_time(right_c),
    times
  )
  expect_equal(
    .extract_surv_time(left_c),
    times
  )
  expect_equal(
    .extract_surv_time(intv_c),
    tibble::tibble(time1 = times, time2 = rep(1.0, 5))
  )
  expect_equal(
    .extract_surv_time(count_c),
    tibble::tibble(start = times, stop = times2)
  )

  expect_equal(
    .extract_surv_status(right_c),
    events
  )
  expect_equal(
    .extract_surv_status(left_c),
    events
  )
  expect_equal(
    .extract_surv_status(intv_c),
    events
  )
  expect_equal(
    .extract_surv_status(count_c),
    events
  )

})
