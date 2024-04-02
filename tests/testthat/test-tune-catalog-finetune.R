# copied from https://github.com/tidymodels/tune/blob/main/tests/testthat/helper-tune-package.R
catalog_lines <- function(pattern, context) {
  local({
    # A local variable; once we've found the line with the intended pattern,
    # don't print it anymore
    found_pattern <- FALSE
    # Return function to pass to `expect_snapshot(transform)`
    function(lines) {
      matches <- grepl(pattern, lines, fixed = TRUE)
      if (any(matches) & !found_pattern & !identical(context, "CI")) {
        found_pattern <<- TRUE
        # Possible that there may be more than one match; return the last
        return(lines[max(which(matches))])
      }

      # Otherwise, we're looking for the unique messages
      lines[grepl("^>", lines)]
    }
  })
}

# `ci_context = "CI"` will test against snapshots in `_snaps/CI`, otherwise
# NULL which will test as usual
ci_context <- switch(Sys.getenv("CI"), "true" = "CI")

test_that("interactive logger works (finetune integration, error)", {
  skip_if(tune:::allow_parallelism(FALSE), "Will not catalog: parallelism is enabled")
  local_mocked_bindings(is_testing = function() {FALSE}, .package = "tune")
  library(finetune)

  raise_error <- function(x) {stop("AHHhH")}
  raise_warning <- function(x) {warning("ope! yikes.")}

  set.seed(1)
  expect_snapshot(
    {res_anova <-
      tune_race_anova(
        parsnip::nearest_neighbor("regression", "kknn", neighbors = tune()),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        control = control_race(extract = function(x) {raise_warning(); raise_error()})
      )},
    transform = catalog_lines("A: x5   B: x5", ci_context),
    variant = ci_context
  )

  set.seed(1)
  expect_snapshot(
    {res_sa <-
      tune_sim_anneal(
        parsnip::nearest_neighbor("regression", "kknn", neighbors = tune()),
        Sale_Price ~ .,
        rsample::vfold_cv(modeldata::ames[, c(72, 40:45)], 5),
        initial = res_anova,
        iter = 15,
        control = control_sim_anneal(verbose_iter = FALSE,
                                     extract = function(x) {raise_warning(); raise_error()})
      )},
    transform = catalog_lines("A: x75   B: x75", ci_context),
    variant = ci_context
  )
})
