test_that('messaging with unknown implementation (bag tree, tidymodels/parsnip#793)', {
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

test_that('messaging with unknown implementation (decision tree, tidymodels/parsnip#793)', {
  skip_if(utils::packageVersion("parsnip") < "1.0.2")

  library(parsnip)

  # one possible extension --------------------------------------------------
  # known engine, mode
  expect_snapshot(
    decision_tree()
  )

  # known, uniquely identifying mode
  expect_snapshot(
    decision_tree() %>%
      set_mode("censored regression")
  )

  # known engine, two possible extensions
  expect_snapshot(
    decision_tree() %>%
      set_engine("partykit")
  )

  # known engine, one possible extension
  expect_snapshot(
    decision_tree() %>%
      set_engine("partykit") %>%
      set_mode("regression")
  )

  # inter-extension interactions --------------------------------------------
  library(censored)

  # do not message -- well-specified spec
  expect_snapshot(
    decision_tree() %>%
      set_mode("censored regression") %>%
      set_engine("rpart")
  )

  # do not message - this could still possibly be a well-specified spec
  expect_snapshot(
    decision_tree() %>%
      set_engine("partykit")
  )

  # message, now that additional mode means this is in bonsai
  expect_snapshot(
    decision_tree() %>%
      set_mode("regression") %>%
      set_engine("partykit")
  )

  # do not message now that bonsai is loaded
  library(bonsai)

  expect_snapshot(
    decision_tree() %>%
      set_mode("regression") %>%
      set_engine("partykit")
  )
})

test_that('missing implementation checks prompt conservatively with old objects (bag_tree)', {
  # tidymodels/parsnip@793 introduced the `user_specified_engine` and
  # `user_specified_mode` slots to parsnip model spec objects. model types defined in external
  # extension packages, as well as model specs generated before parsnip 1.0.2,
  # will not have this slot. ensure that these messages/errors aren't
  # erroneously introduced when that's the case
  skip_if(utils::packageVersion("parsnip") < "1.0.2")

  library(parsnip)

  # bag tree, only implementation in extension ---------------------------------
  bt <-
    bag_tree() %>%
    set_engine("rpart") %>%
    set_mode("regression")

  bt$user_specified_mode <- NULL
  bt$user_specified_engine <- NULL

  expect_snapshot(bt)

  # message goes away after loading needed extension
  library(baguette)

  expect_snapshot(bt)
})

test_that('missing implementation checks prompt conservatively with old objects (decision_tree)', {
  # see comment in above chunk
  skip_if(utils::packageVersion("parsnip") < "1.0.2")

  library(parsnip)

  # decision tree, some implementations in parsnip ---------------------------------
  dt <-
    decision_tree()

  dt$user_specified_mode <- NULL
  dt$user_specified_engine <- NULL

  expect_snapshot(dt)

  # not well-defined with current packages, though from old version
  dt_censored <-
    decision_tree() %>%
    set_mode("censored regression")

  dt_censored$user_specified_mode <- NULL
  dt_censored$user_specified_engine <- NULL

  expect_snapshot(dt_censored)

  # message continues to not appear after loading needed extension
  library(censored)

  expect_snapshot(dt_censored)
})

test_that('missing implementation checks prompt conservatively with external objects (arima_boost)', {
  # see comment in above chunk
  skip_if(utils::packageVersion("parsnip") < "1.0.2")

  library(modeltime)

  # arima boost, model type defined in modeltime ------------------------------
  ab <-
    arima_boost()

  # (these slots don't exist for now, but to ensure they don't in the future)
  ab$user_specified_mode <- NULL
  ab$user_specified_engine <- NULL

  expect_snapshot(ab)
})
