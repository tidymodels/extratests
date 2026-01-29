test_that(".get_split_args() works for spatialsample rsets", {
  skip_if_not_installed("rsample", minimum_version = "1.3.1.9001")

  # do not attach spatialsample, see rsample issue #599
  rs <- spatialsample::spatial_block_cv(spatialsample::boston_canopy, v = 3)
  args <- .get_split_args(rs)
  expect_equal(args$v, 3)
})
