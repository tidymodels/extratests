library(testthat)
library(parsnip)

context("spark descriptors")

source(test_path("parsnip-helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)] %>% as.data.frame()

# ------------------------------------------------------------------------------

context("descriptor variables")

# ------------------------------------------------------------------------------

template <- function(col, pred, ob, lev, fact, dat, x, y) {
  lst <- list(.cols = col, .preds = pred, .obs = ob,
              .lvls = lev, .facts = fact, .dat = dat,
              .x = x, .y = y)

  Filter(Negate(is.null), lst)
}

eval_descrs <- function(descrs, not = NULL) {

  if (!is.null(not)) {
    for (descr in not) {
      descrs[[descr]] <- NULL
    }
  }

  lapply(descrs, do.call, list())
}

class_tab <- table(hpc$class, dnn = NULL)

# ------------------------------------------------------------------------------



test_that("spark descriptor", {

  skip_if_not_installed("sparklyr")

  library(sparklyr)
  library(dplyr)

  sc <- try(spark_connect(master = "local"), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  npk_descr  <- copy_to(sc,  npk[, 1:4],  "npk_descr", overwrite = TRUE)
  hpc_descr <- copy_to(sc,        hpc, "hpc_descr", overwrite = TRUE)

  # spark does not allow .x, .y, .dat; spark handles factors differently
  template2 <- purrr::partial(template, x = NULL, y = NULL, dat = NULL)
  eval_descrs2 <- purrr::partial(eval_descrs, not = c(".x", ".y", ".dat"))
  class_tab2 <- table(as.character(hpc$class), dnn = NULL)

  expect_equal(
    template2(6, 4, 150, NA, 1),
    eval_descrs2(parsnip:::get_descr_form(compounds ~ ., data = hpc_descr))
  )
  expect_equal(
    template2(3, 1, 150, NA, 1),
    eval_descrs2(parsnip:::get_descr_form(compounds ~ class, data = hpc_descr))
  )
  expect_equal(
    template2(1, 1, 150, NA, 0),
    eval_descrs2(parsnip:::get_descr_form(compounds ~ input_fields, data = hpc_descr))
  )
  expect_equivalent(
    template2(4, 4, 150, class_tab2, 0),
    eval_descrs2(parsnip:::get_descr_form(class ~ ., data = hpc_descr))
  )
  expect_equal(
    template2(1, 1, 150, class_tab2, 0),
    eval_descrs2(parsnip:::get_descr_form(class ~ input_fields, data = hpc_descr))
  )
  expect_equivalent(
    template2(7, 3, 24, rev(table(npk$K, dnn = NULL)), 3),
    eval_descrs2(parsnip:::get_descr_form(K ~ ., data = npk_descr))
  )

})
