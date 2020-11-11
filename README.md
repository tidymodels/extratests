
<!-- README.md is generated from README.Rmd. Please edit that file -->

# extratests

<!-- badges: start -->

[![R build status - CRAN
installs](https://github.com/tidymodels/extratests/workflows/CRAN-R-CMD-check/badge.svg)](https://github.com/tidymodels/extratests/actions)
[![R build status - GH
installs](https://github.com/tidymodels/extratests/workflows/GH-R-CMD-check/badge.svg)](https://github.com/tidymodels/extratests/actions)
[![R build status -
Spark](https://github.com/tidymodels/extratests/workflows/spark-R-CMD-check/badge.svg)](https://github.com/tidymodels/extratests/actions)
<!-- badges: end -->

`extratests` is an internal package used for tests that

  - Depend on multiple tidymodels packages

  - Involve special/extra packages.

  - Whose run-time is not practical for individual packages.

These tests are run on a cron job and are run for both CRAN versions and
the current GitHub development versions.
