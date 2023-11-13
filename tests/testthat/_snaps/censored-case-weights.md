# bag_tree - rpart censored case weights

    Code
      wt_fit$fit$call
    Output
      bagging.data.frame(formula = Surv(time, event) ~ ., data = data, 
          weights = weights)

# proportional_hazards - glmnet censored case weights

    Code
      wt_fit$fit$fit$call
    Output
      glmnet::glmnet(x = data_obj$x, y = data_obj$y, family = "cox", 
          weights = weights, alpha = alpha, lambda = lambda)

