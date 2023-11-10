# poisson_reg - glmnet case weights

    Code
      print(wt_fit$fit$call)
    Output
      glmnet::glmnet(x = maybe_matrix(x), y = y, family = "poisson", 
          weights = weights, lambda = ~10^(-4:-1))

# poisson_reg - stan case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glm(formula = art ~ ., family = stats::poisson, 
          data = data, weights = weights, seed = ~1, refresh = ~0)

