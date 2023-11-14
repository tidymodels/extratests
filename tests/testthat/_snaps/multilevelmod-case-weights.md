# linear_reg - stan_glmer case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glmer(formula = value ~ (1 | id), data = data, 
          family = stats::gaussian, weights = weights, refresh = 0)

# linear_reg - lme4::lmer case weights

    Code
      print(wt_fit$fit@call)
    Output
      lme4::lmer(formula = value ~ (1 | id), data = data, weights = weights)

# logistic_reg - stan_glmer case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glmer(formula = Class ~ A + B + (1 | id), data = data, 
          family = stats::binomial, weights = weights, seed = ~1, refresh = 0)

# logistic_reg - lme4::glmer case weights

    Code
      print(wt_fit$fit@call)
    Output
      lme4::glmer(formula = Class ~ A + B + (1 | id), data = data, 
          family = binomial, weights = weights)

# poisson_reg - stan_glmer case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glmer(formula = art ~ (1 | id), data = data, family = stats::poisson, 
          weights = weights, seed = ~1, refresh = 0)

# poisson_reg - lme4::glmer case weights

    Code
      print(wt_fit$fit@call)
    Output
      lme4::glmer(formula = art ~ (1 | id), data = data, family = stats::poisson, 
          weights = weights)

