skip_if_no_spark()

library(testthat)
library(parsnip)

hpc <- hpc_data[1:150, c(2:5, 8)] %>% as.data.frame()

# ------------------------------------------------------------------------------

template <- function(col, pred, ob, lev, fact, dat, x, y) {
  lst <- list(
    .cols = col,
    .preds = pred,
    .obs = ob,
    .lvls = lev,
    .facts = fact,
    .dat = dat,
    .x = x,
    .y = y
  )

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
  sc <- spark_test_connection()

  npk_descr <- copy_to(sc, npk[, 1:4], "npk_descr", overwrite = TRUE)
  hpc_descr <- copy_to(sc, hpc, "hpc_descr", overwrite = TRUE)

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
    eval_descrs2(parsnip:::get_descr_form(
      compounds ~ input_fields,
      data = hpc_descr
    ))
  )
  expect_equal(
    template2(4, 4, 150, class_tab2, 0),
    eval_descrs2(parsnip:::get_descr_form(class ~ ., data = hpc_descr)),
    ignore_attr = TRUE
  )
  expect_equal(
    template2(1, 1, 150, class_tab2, 0),
    eval_descrs2(parsnip:::get_descr_form(
      class ~ input_fields,
      data = hpc_descr
    ))
  )
  expect_equal(
    template2(7L, 3L, 24L, rev(table(npk$K, dnn = NULL)), 3L),
    eval_descrs2(parsnip:::get_descr_form(K ~ ., data = npk_descr)),
    ignore_attr = TRUE
  )
})
