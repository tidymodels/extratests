test_that('grf classification', {
  skip_if_not_installed("parsnip", minimum_version = "1.3.3.9000")
  skip_if_not_installed("grf")
  skip_if_not_installed("modeldata")

  scat_dat <- modeldata::scat
  scat_tr <- scat_dat[1:94, ]
  scat_te <- scat_dat[95:110, ]

  expect_snapshot(
    rand_forest(mtry = 100, trees = 1, min_n = 1000) |>
      set_engine("grf") |>
      set_mode("classification") |>
      translate()
  )

  expect_snapshot_warning(
    rand_forest(mtry = 100) |>
      set_engine("grf") |>
      set_mode("classification") |>
      fit(Species ~ ., data = scat_tr)
  )

  grf_spec <-
    rand_forest() |>
    set_engine("grf", seed = 1) |>
    set_mode("classification")

  set.seed(281)
  grf_fit <- fit(grf_spec, Species ~ ., data = scat_tr)
  expect_s3_class(grf_fit$fit, "probability_forest")

  ###

  grf_cls <- predict(grf_fit, scat_te)
  expect_equal(nrow(grf_cls), nrow(scat_te))
  expect_equal(
    grf_cls[0, ],
    structure(
      list(
        .pred_class = structure(
          integer(0),
          levels = c("bobcat", "coyote", "gray_fox"),
          class = "factor"
        )
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  ###

  grf_prb <- predict(grf_fit, scat_te, type = "prob")
  expect_equal(nrow(grf_prb), nrow(scat_te))
  expect_equal(
    grf_prb[0, ],
    structure(
      list(
        .pred_bobcat = numeric(0),
        .pred_coyote = numeric(0),
        .pred_gray_fox = numeric(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  ###

  grf_ci <- predict(grf_fit, scat_te, type = "conf_int")
  expect_equal(nrow(grf_ci), nrow(scat_te))
  expect_equal(
    grf_ci[0, ],
    structure(
      list(
        .pred_lower_bobcat = numeric(0),
        .pred_lower_coyote = numeric(0),
        .pred_lower_gray_fox = numeric(0),
        .pred_upper_bobcat = numeric(0),
        .pred_upper_coyote = numeric(0),
        .pred_upper_gray_fox = numeric(0)
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"),
      level = 0.95
    )
  )
})

test_that('grf classification with case weights', {
  skip_if_not_installed("parsnip", minimum_version = "1.3.3.9000")
  skip_if_not_installed("grf")
  skip_if_not_installed("modeldata")

  scat_dat <- modeldata::scat
  scat_tr <- scat_dat[1:94, ]
  scat_te <- scat_dat[95:110, ]

  set.seed(281)
  scat_cw <-
    scat_tr |>
    mutate(
      wts = hardhat::importance_weights(runif(nrow(scat_tr)))
    )
  not_missing <- complete.cases(scat_cw)

  set.seed(281)
  grf_wt_fit <- fit(
    grf_spec,
    Species ~ .,
    data = scat_cw |> select(-wts),
    case_weights = scat_cw$wts[not_missing]
  )
  expect_s3_class(grf_wt_fit$fit, "probability_forest")
  expect_false(
    isTRUE(
      all.equal(
        grf_wt_fit$fit$predictions,
        grf_fit$fit$predictions
      )
    )
  )
  expect_equal(
    grf_wt_fit$fit$sample.weights,
    scat_cw$wts[not_missing]
  )

  set.seed(281)
  grf_wf_fit <-
    workflow(Species ~ ., grf_spec) |>
    add_case_weights(wts) |>
    fit(scat_cw)

  expect_false(
    isTRUE(
      all.equal(
        grf_fit$fit$predictions,
        grf_wf_fit$fit$fit$fit$predictions
      )
    )
  )
  expect_equal(
    grf_wf_fit$fit$fit$fit$sample.weights,
    scat_cw$wts
  )
})

test_that('grf regression', {
  skip_if_not_installed("parsnip", minimum_version = "1.3.3.9000")
  skip_if_not_installed("grf")
  skip_if_not_installed("modeldata")

  ames_dat <- modeldata::ames[1:100, ]
  ames_tr <- ames_dat[1:90, ]
  ames_te <- ames_dat[91:100, ]

  expect_snapshot(
    rand_forest(mtry = 100, trees = 1, min_n = 1000) |>
      set_engine("grf") |>
      set_mode("regression") |>
      translate()
  )

  grf_spec <-
    rand_forest() |>
    set_engine("grf", seed = 101010101) |>
    set_mode("regression")

  set.seed(281)
  grf_fit <- fit(grf_spec, Sale_Price ~ ., data = ames_tr)
  expect_s3_class(grf_fit$fit, "regression_forest")

  ###

  grf_num <- predict(grf_fit, ames_te)
  expect_equal(nrow(grf_num), nrow(ames_te))
  expect_equal(
    grf_num[0, ],
    structure(
      list(.pred = double(0)),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  ###

  grf_ci <- predict(grf_fit, ames_te, type = "conf_int")
  expect_equal(nrow(grf_ci), nrow(ames_te))
  expect_equal(
    grf_ci[0, ],
    structure(
      list(.pred_lower = double(0), .pred_upper = double(0)),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame"),
      level = 0.95
    )
  )
})

test_that('grf regression with case weights', {
  skip_if_not_installed("parsnip", minimum_version = "1.3.3.9000")
  skip_if_not_installed("grf")
  skip_if_not_installed("modeldata")

  ames_dat <- modeldata::ames[1:100, ]
  ames_tr <- ames_dat[1:90, ]
  ames_te <- ames_dat[91:100, ]

  set.seed(281)
  ames_cw <-
    ames_tr |>
    mutate(
      wts = hardhat::importance_weights(runif(nrow(ames_tr)))
    )

  set.seed(281)
  grf_wt_fit <- fit(
    grf_spec,
    Sale_Price ~ .,
    data = ames_cw |> select(-wts),
    case_weights = ames_cw$wts
  )
  expect_s3_class(grf_wt_fit$fit, "regression_forest")
  expect_false(
    isTRUE(
      all.equal(
        grf_wt_fit$fit$predictions,
        grf_fit$fit$predictions
      )
    )
  )
  expect_equal(
    grf_wt_fit$fit$sample.weights,
    ames_cw$wts
  )

  set.seed(281)
  grf_wf_fit <-
    workflow(Sale_Price ~ ., grf_spec) |>
    add_case_weights(wts) |>
    fit(ames_cw)

  expect_false(
    isTRUE(
      all.equal(
        grf_fit$fit$predictions,
        grf_wf_fit$fit$fit$fit$predictions
      )
    )
  )
  expect_equal(
    grf_wf_fit$fit$fit$fit$sample.weights,
    ames_cw$wts
  )
})

test_that('grf quantile regression', {
  skip_if_not_installed("parsnip", minimum_version = "1.3.3.9000")
  skip_if_not_installed("grf")
  skip_if_not_installed("modeldata")

  ames_dat <- modeldata::ames[1:100, ]
  ames_tr <- ames_dat[1:90, ]
  ames_te <- ames_dat[91:100, ]

  expect_snapshot(
    rand_forest(mtry = 100, trees = 1, min_n = 1000) |>
      set_engine("grf") |>
      set_mode("quantile regression", quantile_levels = (1:3) / 4) |>
      translate()
  )

  grf_spec <-
    rand_forest() |>
    set_engine("grf") |>
    set_mode("quantile regression", quantile_levels = (1:3) / 4)

  set.seed(281)
  grf_fit <- fit(grf_spec, Sale_Price ~ ., data = ames_tr)
  expect_s3_class(grf_fit$fit, "quantile_forest")

  ###

  grf_qtl <- predict(grf_fit, ames_te)
  expect_equal(nrow(grf_qtl), nrow(ames_te))
  expect_equal(
    grf_qtl[0, ],
    structure(
      list(
        .pred_quantile = structure(
          list(),
          quantile_levels = c(0.25, 0.5, 0.75),
          class = c("quantile_pred", "vctrs_vctr", "list")
        )
      ),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
})
