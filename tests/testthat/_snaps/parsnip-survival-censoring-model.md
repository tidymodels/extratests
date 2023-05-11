# print reverse Kaplan-Meier curves

    Code
      mod_fit$censor_probs
    Output
      $formula
      Surv(time, status) ~ age + sex
      <environment: base>
      
      $fit
      
      Call: prodlim::prodlim(formula = Surv(time, status) ~ 1, data = eval_env$data, 
          reverse = TRUE, x = FALSE, type = "surv")
      
    Message <simpleMessage>
      Kaplan-Meier estimator for the censoring time survival function
    Output
      
    Message <simpleMessage>
      No covariates
    Output
      
      Right-censored response of a survival model
      
      No.Observations: 228 
      
      Pattern:
                      Freq
       event          165 
       right.censored 63  
      
      $label
      [1] "reverse_km"
      
      $required_pkgs
      [1] "prodlim"
      
      attr(,"class")
      [1] "censoring_model_reverse_km" "censoring_model"           

# Handle unknown censoring model

    Code
      predict(alt_obj, time = 100)
    Error <simpleError>
      no applicable method for 'predict' applied to an object of class "censoring_model"

