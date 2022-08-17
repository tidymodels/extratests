test_that('message informatively with unknown implementation (tidymodels/parsnip#793)', {
  skip_if(utils::packageVersion("parsnip") < "1.0.2")

  library(parsnip)

  # one possible extension --------------------------------------------------
  # known engine, mode
  expect_snapshot(
    bag_tree() %>%
      set_engine("rpart") %>%
      set_mode("regression")
  )

  # known, uniquely identifying mode
  expect_snapshot(
    bag_tree() %>%
      set_mode("censored regression")
  )

  # two possible extensions -------------------------------------------------
  # all default / unknown
  expect_snapshot(
    bag_tree()
  )

  # extension-ambiguous engine
  expect_snapshot(
    bag_tree() %>%
      set_engine("rpart")
  )

  # inter-extension interactions --------------------------------------------
  library(censored)

  # do not message -- well-specified spec
  expect_snapshot(
    bag_tree() %>%
      set_mode("censored regression") %>%
      set_engine("rpart")
  )

  # do not message - this could still possibly be a well-specified spec
  expect_snapshot(
    bag_tree() %>%
      set_engine("rpart")
  )

  # message as before, even though there is now a different possible bag_tree spec
  expect_snapshot(
    bag_tree() %>%
      set_mode("regression") %>%
      set_engine("rpart")
  )

  expect_snapshot(
    bag_tree() %>%
      set_mode("classification") %>%
      set_engine("C5.0")
  )

  # do not message now that baguette is loaded
  library(baguette)

  expect_snapshot(
    bag_tree() %>%
      set_engine("C5.0")
  )
})
