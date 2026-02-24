# Helpers used for parsnip case weight tests

expect_unequal <-
  function(
    object,
    expected,
    ...,
    tolerance = if (edition_get() >= 3) testthat_tolerance()
  ) {
    expect_true(!compare(object, expected, tolerance = tolerance, ...)$equal)
  }

make_two_class_wts <- function(x) {
  data(two_class_dat, package = "modeldata", envir = rlang::current_env())
  set.seed(1)
  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  two_class_subset <- two_class_dat[wts != 0, ]
  wts <- importance_weights(wts)
  list(wts = wts, subset = two_class_subset)
}

make_ames_wts <- function(x) {
  data("ames", package = "modeldata", envir = rlang::current_env())
  ames$Sale_Price <- log10(ames$Sale_Price)
  ames <- dplyr::select(ames, Sale_Price, Longitude, Latitude, Neighborhood)

  set.seed(1)
  wts <- runif(nrow(ames))
  wts <- ifelse(wts < 1 / 3, 0L, 1L)
  ames_subset <- ames[wts != 0, ]
  wts <- frequency_weights(wts)
  list(wts = wts, subset = ames_subset, full = ames)
}

make_msa_wts <- function(x) {
  data("msa_data", package = "multilevelmod", envir = rlang::current_env())
  set.seed(1)
  wts <- runif(nrow(msa_data))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  msa_data_subset <- msa_data[wts != 0, ]
  wts <- importance_weights(wts)
  list(wts = wts, subset = msa_data_subset)
}

make_penguin_wts <- function() {
  data("penguins", package = "modeldata", envir = rlang::current_env())
  penguins <- penguins[complete.cases(penguins), ]

  set.seed(1)
  wts <- runif(nrow(penguins))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  penguins_subset <- penguins[wts != 0, ]
  wts <- importance_weights(wts)
  list(wts = wts, subset = penguins_subset)
}

make_biochem_wts <- function() {
  data(bioChemists, package = "pscl", envir = rlang::current_env())

  set.seed(1)
  wts <- runif(nrow(bioChemists))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  bioChemists_subset <- bioChemists[wts != 0, ]
  wts <- importance_weights(wts)

  list(wts = wts, subset = bioChemists_subset, full = bioChemists)
}

make_mtcars_wts <- function() {
  set.seed(1)
  wts <- runif(nrow(mtcars))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  mtcars_subset <- mtcars[wts != 0, ]
  wts <- importance_weights(wts)

  list(wts = wts, subset = mtcars_subset)
}

make_cens_wts <- function() {
  data(time_to_million, package = "censored", envir = rlang::current_env())

  set.seed(1)
  time_to_million <- time_to_million[
    1:100,
    c("time", "event", "released_theaters", "rated")
  ]
  wts <- runif(nrow(time_to_million))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  cens_subset <- time_to_million[wts != 0, ]
  wts <- importance_weights(wts)

  list(wts = wts, subset = cens_subset, full = time_to_million)
}
