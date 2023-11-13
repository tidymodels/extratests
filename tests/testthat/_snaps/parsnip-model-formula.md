# error without model formula (workflow, no tune)

    Code
      gam_fit <- gam_wflow %>% fit(mtcars)
    Condition
      Error:
      ! When working with generalized additive models, please supply the model specification to `workflows::add_model()` along with a `formula` argument.
      i See `?parsnip::model_formula()` to learn more.

# error without model formula (workflow, with tune)

    Code
      show_notes(gam_res)
    Output
      unique notes:
      --------------------------------------------------------------------------------
      Error:
      ! When working with generalized additive models, please supply the model specification to `workflows::add_model()` along with a `formula` argument.
      i See `?parsnip::model_formula()` to learn more.

# error without model formula (no workflow, with tune)

    Code
      show_notes(gam_res)
    Output
      unique notes:
      --------------------------------------------------------------------------------
      Error:
      ! When working with generalized additive models, please supply the model specification to `workflows::add_model()` along with a `formula` argument.
      i See `?parsnip::model_formula()` to learn more.

