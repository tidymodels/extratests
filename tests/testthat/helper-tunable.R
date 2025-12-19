check_tunable_tibble <- function(x) {
  expect_identical(
    names(x),
    c("name", "call_info", "source", "component", "component_id")
  )
  expect_identical(class(x$name), "character")
  expect_identical(class(x$call_info), "list")
  expect_identical(class(x$source), "character")
  expect_identical(class(x$component), "character")
  expect_identical(class(x$component_id), "character")
  invisible(TRUE)
}
