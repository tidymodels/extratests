# bag_tree - rpart case weights

    Code
      print(wt_fit$fit$call)
    Output
      NULL

# decision_tree - rpart case weights

    Code
      print(wt_fit$fit$call)
    Output
      rpart::rpart(formula = Class ~ ., data = data, weights = weights)

# discrim_flexible - earth case weights

    Code
      print(wt_fit$fit$call)
    Output
      mda::fda(formula = Class ~ ., data = data, weights = weights, 
          method = earth::earth, pmethod = ~"none")

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

# logistic_reg - stan case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glm(formula = Class ~ ., family = stats::binomial, 
          data = data, weights = weights, seed = ~1, refresh = 0)

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

# mars - earth case weights

    Code
      print(wt_fit$fit$call)
    Output
      earth(formula = Class ~ ., data = data, weights = weights, keepxy = TRUE, 
          glm = ~list(family = stats::binomial))

# poisson_reg - stan_glmer case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glmer(formula = art ~ (1 | id), data = data, family = stats::poisson, 
          weights = weights, seed = ~1, refresh = 0)

# poisson_reg - glmnet case weights

    Code
      print(wt_fit$fit$call)
    Output
      glmnet::glmnet(x = maybe_matrix(x), y = y, family = "poisson", 
          weights = weights, path_values = ~10^(-4:-1))

# poisson_reg - stan case weights

    Code
      print(wt_fit$fit$call)
    Output
      rstanarm::stan_glm(formula = art ~ ., family = stats::poisson, 
          data = data, weights = weights, seed = ~1, refresh = ~0)

# poisson_reg - lme4::glmer case weights

    Code
      print(wt_fit$fit@call)
    Output
      lme4::glmer(formula = art ~ (1 | id), data = data, family = stats::poisson, 
          weights = weights)

# rand_forest - ranger case weights

    Code
      print(wt_fit$fit$call)
    Output
      ranger::ranger(x = maybe_data_frame(x), y = y, num.threads = 1, 
          verbose = FALSE, seed = sample.int(10^5, 1), probability = TRUE, 
          case.weights = weights)

