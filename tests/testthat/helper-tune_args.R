check_tune_args_tibble <- function(x) {
  expect_identical(
    names(x),
    c("name", "tunable", "id", "source", "component", "component_id")
  )
  expect_identical(class(x$name), "character")
  expect_identical(class(x$tunable), "logical")
  expect_identical(class(x$id), "character")
  expect_identical(class(x$source), "character")
  expect_identical(class(x$component), "character")
  expect_identical(class(x$component_id), "character")
  expect_true(!any(duplicated(x$id)))
  invisible(TRUE)
}
