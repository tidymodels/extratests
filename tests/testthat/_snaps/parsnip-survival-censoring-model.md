# print reverse Kaplan-Meier curves

    Code
      mod_fit$censor_probs
    Output
      reverse_km model for predicting the probability of censoring

# Handle unknown censoring model

    Code
      predict(alt_obj, time = 100)
    Condition
      Error in `predict()`:
      ! Don't know how to predict with a censoring model of type: reverse_km

