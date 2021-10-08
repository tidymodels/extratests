check_tune_args_tibble <- function(x) {
  expect_equal(names(x), c("name", "tunable", "id", "source", "component", "component_id"))
  expect_equal(class(x$name), "character")
  expect_equal(class(x$tunable), "logical")
  expect_equal(class(x$id), "character")
  expect_equal(class(x$source), "character")
  expect_equal(class(x$component), "character")
  expect_equal(class(x$component_id), "character")
  expect_true(!any(duplicated(x$id)))
  invisible(TRUE)
}
