# bag_tree - rpart censored case weights

    Code
      wt_fit$fit$call
    Output
      bagging.data.frame(formula = Surv(time, event) ~ ., data = data, 
          weights = weights, cp = ~0, minsplit = ~2)

# proportional_hazards - glmnet censored case weights

    Code
      wt_fit$fit$call
    Output
      glmnet::glmnet(x = data_obj$x, y = data_obj$y, family = "cox", 
          weights = weights, alpha = alpha, lambda = lambda)

