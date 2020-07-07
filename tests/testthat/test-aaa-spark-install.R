context("spark installation")

test_that('is spark installed', {
  expect_true(sparklyr::spark_install_find()$installed)
})
