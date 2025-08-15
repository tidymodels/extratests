check_tunable_tibble <- function(x) {
  expect_equal(
    names(x),
    c("name", "call_info", "source", "component", "component_id")
  )
  expect_equal(class(x$name), "character")
  expect_equal(class(x$call_info), "list")
  expect_equal(class(x$source), "character")
  expect_equal(class(x$component), "character")
  expect_equal(class(x$component_id), "character")
  invisible(TRUE)
}
