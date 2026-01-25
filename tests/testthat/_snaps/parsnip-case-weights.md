# boost_tree - xgboost case weights

    Code
      print(attr(wt_fit$fit, "call"))
    Output
      xgboost::xgb.train(params = list(eta = 0.3, max_depth = 6, gamma = 0, 
          colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
          subsample = 1, nthread = 1, objective = "binary:logistic"), 
          data = x$data, nrounds = 15, evals = x$watchlist, verbose = 0)

# decision_tree - rpart case weights

    Code
      print(wt_fit$fit$call)
    Output
      rpart::rpart(formula = Class ~ ., data = data, weights = weights)

# logistic_reg - stan case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glm(formula = Class ~ ., family = stats::binomial, 
          data = data, weights = weights, seed = ~1, refresh = 0)

# mars - earth case weights

    Code
      print(wt_fit$fit$call)
    Output
      earth(formula = Class ~ ., data = data, weights = weights, keepxy = TRUE, 
          glm = ~list(family = stats::binomial))

# rand_forest - ranger case weights

    Code
      print(wt_fit$fit$call)
    Output
      ranger::ranger(x = maybe_data_frame(x), y = y, num.threads = 1, 
          verbose = FALSE, seed = sample.int(10^5, 1), probability = TRUE, 
          case.weights = weights)

