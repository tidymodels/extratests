test_that('survival helpers', {
  skip_if_not_installed("parsnip", minimum_version = "1.0.4.9001")
  library(survival)

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

  expect_snapshot(
    error = TRUE,
    parsnip:::.check_cens_type(left_c, fail = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    parsnip:::.check_cens_type(left_c, type = c("right", "interval"), fail = TRUE)
  )

  expect_true(parsnip:::.is_censored_right(right_c))
  expect_false(parsnip:::.is_censored_right(left_c))

  expect_equal(
    parsnip:::.extract_surv_time(right_c),
    times
  )
  expect_equal(
    parsnip:::.extract_surv_time(left_c),
    times
  )
  expect_equal(
    parsnip:::.extract_surv_time(intv_c),
    tibble::tibble(time1 = times, time2 = rep(1.0, 5))
  )
  expect_equal(
    parsnip:::.extract_surv_time(count_c),
    tibble::tibble(start = times, stop = times2)
  )

  expect_equal(
    parsnip:::.extract_surv_status(right_c),
    events
  )
  expect_equal(
    parsnip:::.extract_surv_status(left_c),
    events
  )
  expect_equal(
    parsnip:::.extract_surv_status(intv_c),
    events
  )
  expect_equal(
    parsnip:::.extract_surv_status(count_c),
    events
  )

  # ------------------------------------------------------------------------------

  events_interval_full <- c(1, 0:3)
  events_interval_12 <- c(1, 2, 1, 2, 1)

  intv_c <- Surv(times, times2, events_interval_full, type = "interval")
  intv_c_12 <- Surv(times, times2, events_interval_12, type = "interval")

  expect_equal(
    parsnip:::.extract_surv_time(intv_c),
    tibble::tibble(time1 = times, time2 = c(rep(1.0, 4), 200))
  )

  expect_equal(
    parsnip:::.extract_surv_status(intv_c),
    events_interval_full
  )
  expect_equal(
    parsnip:::.extract_surv_status(intv_c_12),
    events_interval_12
  )

})
