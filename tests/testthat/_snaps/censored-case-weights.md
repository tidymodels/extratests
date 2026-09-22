# proportional_hazards - glmnet censored case weights

    Code
      wt_fit$fit$call
    Output
      glmnet::glmnet(x = data_obj$x, y = data_obj$y, family = "cox", 
          weights = weights, alpha = alpha, lambda = lambda, cox.ties = ..1)

# rand_forest - ranger censored case weights

    Code
      wt_fit$call
    Output
      ranger::ranger(formula = Surv(time, event) ~ ., data = data, 
          num.threads = 1, verbose = FALSE, seed = sample.int(10^5, 
              1), case.weights = weights)

# rand_forest - randomForestSRC censored case weights

    Code
      wt_fit$call
    Output
      randomForestSRC::rfsrc(formula = Surv(..y_time, ..y_status) ~ 
          ., data = data, case.wt = weights)

