skip_if_not_installed("survival")

test_that(".is_surv()", {
  skip_if_not_installed("parsnip", minimum_version = "1.2.1.9002")
  times <- seq(1, 100, length.out = 5)
  events <- c(1, 0, 1, 0, 1)
  right_c <- survival::Surv(times, events)

  expect_true(parsnip:::.is_surv(right_c))
  expect_false(parsnip:::.is_surv(1, fail = FALSE))
  expect_snapshot(error = TRUE, parsnip:::.is_surv(1))
})

test_that(".check_cens_type()", {
  skip_if_not_installed("parsnip", minimum_version = "1.2.1.9002")
  times <- seq(1, 100, length.out = 5)
  events <- c(1, 0, 1, 0, 1)
  left_c <- survival::Surv(times, events, type = "left")

  expect_snapshot(error = TRUE, {
    parsnip:::.check_cens_type(left_c, type = "right", fail = TRUE)
  })
  expect_snapshot(error = TRUE, {
    parsnip:::.check_cens_type(
      left_c,
      type = c("right", "interval"),
      fail = TRUE
    )
  })
})

test_that(".is_censored_right()", {
  times <- seq(1, 100, length.out = 5)
  events <- c(1, 0, 1, 0, 1)
  right_c <- survival::Surv(times, events)
  left_c <- survival::Surv(times, events, type = "left")

  expect_true(parsnip:::.is_censored_right(right_c))
  expect_false(parsnip:::.is_censored_right(left_c))
})

test_that(".extract_surv_time()", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9003")

  times <- seq(1, 100, length.out = 5)
  times2 <- seq(100, 200, length.out = 5)
  events <- c(1, 0, 1, 0, 1)

  right_c <- survival::Surv(times, events)
  left_c <- survival::Surv(times, events, type = "left")
  intv_c <- survival::Surv(times, times2, events, type = "interval")
  count_c <- survival::Surv(times, times2, events)

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
    parsnip:::.extract_surv_time(right_c[1]),
    times[1]
  )
  expect_equal(
    parsnip:::.extract_surv_time(left_c[1]),
    times[1]
  )
  expect_equal(
    parsnip:::.extract_surv_time(intv_c[1]),
    tibble::tibble(time1 = times[1], time2 = 1.0)
  )
  expect_equal(
    parsnip:::.extract_surv_time(count_c[1]),
    tibble::tibble(start = times[1], stop = times2[1])
  )
})


test_that(".extract_surv_time() vector results are unnamed", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9002")

  times <- seq(1, 100, length.out = 5)
  events <- c(1, 0, 1, 0, 1)

  right_c <- survival::Surv(times, events)
  left_c <- survival::Surv(times, events, type = "left")

  expect_named(parsnip:::.extract_surv_time(right_c), NULL)
  expect_named(parsnip:::.extract_surv_time(left_c), NULL)

  # single observation
  expect_named(parsnip:::.extract_surv_time(right_c[1]), NULL)
  expect_named(parsnip:::.extract_surv_time(left_c[1]), NULL)
})

test_that(".extract_surv_status()", {
  times <- seq(1, 100, length.out = 5)
  times2 <- seq(100, 200, length.out = 5)
  events <- c(1, 0, 1, 0, 1)

  right_c <- survival::Surv(times, events)
  left_c <- survival::Surv(times, events, type = "left")
  intv_c <- survival::Surv(times, times2, events, type = "interval")
  count_c <- survival::Surv(times, times2, events)

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
})

test_that(".extract_surv_status() results are unnamed", {
  skip_if_not_installed("parsnip", minimum_version = "1.1.1.9002")

  times <- seq(1, 100, length.out = 5)
  times2 <- seq(100, 200, length.out = 5)
  events <- c(1, 0, 1, 0, 1)

  right_c <- survival::Surv(times, events)
  left_c <- survival::Surv(times, events, type = "left")
  intv_c <- survival::Surv(times, times2, events, type = "interval")
  count_c <- survival::Surv(times, times2, events)

  expect_named(parsnip:::.extract_surv_status(right_c), NULL)
  expect_named(parsnip:::.extract_surv_status(left_c), NULL)
  expect_named(parsnip:::.extract_surv_status(intv_c), NULL)
  expect_named(parsnip:::.extract_surv_status(count_c), NULL)

  # single observation
  expect_named(parsnip:::.extract_surv_status(right_c[1]), NULL)
  expect_named(parsnip:::.extract_surv_status(left_c[1]), NULL)
  expect_named(parsnip:::.extract_surv_status(intv_c[1]), NULL)
  expect_named(parsnip:::.extract_surv_status(count_c[1]), NULL)
})

test_that(".extract_surv_status() does not transform status for interval censoring", {
  times <- seq(1, 100, length.out = 5)
  times2 <- seq(100, 200, length.out = 5)
  events_interval_full <- c(1, 0:3)
  events_interval_12 <- c(1, 2, 1, 2, 1)

  intv_c <- survival::Surv(
    times,
    times2,
    events_interval_full,
    type = "interval"
  )
  intv_c_12 <- survival::Surv(
    times,
    times2,
    events_interval_12,
    type = "interval"
  )

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
