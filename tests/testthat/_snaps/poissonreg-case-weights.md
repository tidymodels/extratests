# poisson_reg - glmnet case weights

    Code
      print(wt_fit$fit$call)
    Output
      glmnet::glmnet(x = maybe_matrix(x), y = y, family = "poisson", 
          weights = weights, lambda = c(1e-04, 0.001, 0.01, 0.1))

# poisson_reg - stan case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glm(formula = art ~ ., family = stats::poisson, 
          data = data, weights = weights, seed = 1, refresh = 0)

