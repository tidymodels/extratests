# grf classification

    Code
      translate(set_mode(set_engine(rand_forest(mtry = 100, trees = 1, min_n = 1000),
      "grf"), "classification"))
    Output
      Random Forest Model Specification (classification)
      
      Main Arguments:
        mtry = 100
        trees = 1
        min_n = 1000
      
      Computational engine: grf 
      
      Model fit template:
      grf::probability_forest(X = missing_arg(), Y = missing_arg(), 
          weights = missing_arg(), mtry = min_cols(~100, x), num.trees = 1, 
          min.node.size = min_rows(~1000, x), num.threads = 1)

---

    ! 100 columns were requested but there were 29 predictors in the data.
    i 29 predictors will be used.

# grf regression

    Code
      translate(set_mode(set_engine(rand_forest(mtry = 100, trees = 1, min_n = 1000),
      "grf"), "regression"))
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = 100
        trees = 1
        min_n = 1000
      
      Computational engine: grf 
      
      Model fit template:
      grf::regression_forest(X = missing_arg(), Y = missing_arg(), 
          weights = missing_arg(), mtry = min_cols(~100, x), num.trees = 1, 
          min.node.size = min_rows(~1000, x), num.threads = 1)

# grf quantile regression

    Code
      translate(set_mode(set_engine(rand_forest(mtry = 100, trees = 1, min_n = 1000),
      "grf"), "quantile regression", quantile_levels = (1:3) / 4))
    Output
      Random Forest Model Specification (quantile regression)
      
      Main Arguments:
        mtry = 100
        trees = 1
        min_n = 1000
      
      Computational engine: grf 
      
      Model fit template:
      grf::quantile_forest(X = missing_arg(), Y = missing_arg(), mtry = min_cols(~100, 
          x), num.trees = 1, min.node.size = min_rows(~1000, x), num.threads = 1, 
          quantiles = quantile_levels)
    Message
      Quantile levels: 0.25, 0.5, and 0.75.

