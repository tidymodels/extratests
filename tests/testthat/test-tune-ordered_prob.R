test_that("tune works with ordered_prob metrics", {
  skip_if_not_installed("tune", "2.0.1.9003")
  skip_if_not_installed("parsnip", "1.4.1.9003")
  skip_if_not_installed("yardstick", "1.3.2.9000")

  set.seed(1234)

  # To avoid "No observations were detected in `truth` for level: High." warning
  house_data <- MASS::housing[
    rep(seq(nrow(MASS::housing)), MASS::housing$Freq),
    -5
  ]
  house_split <- rsample::initial_split(house_data, prop = .8)
  house_train <- rsample::training(house_split)
  house_test <- rsample::testing(house_split)

  # prep for fit & tuning
  house_rec <- recipes::recipe(Sat ~ Infl + Type + Cont, data = house_train)
  house_spec <- parsnip::multinom_reg() |>
    parsnip::set_engine("nnet") |>
    parsnip::set_args(penalty = hardhat::tune())

  set <- yardstick::metric_set(yardstick::roc_auc, yardstick::ranked_prob_score)

  expect_no_message(
    house_res <- tune::tune_grid(
      house_spec,
      preprocessor = house_rec,
      resamples = rsample::vfold_cv(house_train),
      grid = 3,
      metrics = set
    )
  )

  expect_identical(
    nrow(collect_metrics(house_res)),
    3L * 2L
  )
})
