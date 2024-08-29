# print reverse Kaplan-Meier curves

    Code
      mod_fit$censor_probs
    Output
      reverse_km model for predicting the probability of censoring

# predict.censoring_model_reverse_km checks empty dots

    Code
      predict(mod_fit$censor_probs, time = pred_times, invalid_arg = TRUE)
    Condition
      Error in `predict()`:
      ! `...` must be empty.
      x Problematic argument:
      * invalid_arg = TRUE

# Handle unknown censoring model

    Code
      predict(alt_obj, time = 100)
    Condition
      Error in `predict()`:
      ! Don't know how to predict with a censoring model of type reverse_km.

