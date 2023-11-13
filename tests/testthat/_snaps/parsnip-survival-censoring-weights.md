# error messages in context of .censoring_weights_graf()

    Code
      .censoring_weights_graf("nothing useful")
    Condition
      Error in `.censoring_weights_graf()`:
      ! There is no `.censoring_weights_graf()` method for objects with class(es): 'character'

---

    Code
      .censoring_weights_graf(workflows::workflow())
    Condition
      Error in `.censoring_weights_graf()`:
      ! The workflow does not have a model fit object.

---

    Code
      .censoring_weights_graf(wrong_model, lung2)
    Condition
      Error in `.check_censor_model()`:
      ! The model needs to be for mode 'censored regression', not for mode 'regression'.

---

    Code
      .censoring_weights_graf(cox_model, lung)
    Condition
      Error:
      ! There should be a single column of class `Surv`

---

    Code
      lung_left <- lung[1, , drop = FALSE]
      lung_left$surv <- Surv(10, 0, type = "left")
      .censoring_weights_graf(cox_model, lung_left)
    Condition
      Error in `.censoring_weights_graf()`:
      ! For this usage, the allowed censoring type is: 'right'

---

    Code
      .censoring_weights_graf(cox_model, lung2)
    Condition
      Error:
      ! The input should have a list column called `.pred`.

---

    Code
      .censoring_weights_graf(cox_model, preds, cens_predictors = "shouldn't be using this anyway!")
    Condition
      Warning:
      The 'cens_predictors' argument to the survival weighting function is not currently used.
    Output
      # A tibble: 3 x 2
        .pred              surv
        <list>           <Surv>
      1 <tibble [2 x 5]>   306 
      2 <tibble [2 x 5]>   455 
      3 <tibble [2 x 5]>  1010+

