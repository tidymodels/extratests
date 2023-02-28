# bag_tree - rpart censored case weights

    Code
      wt_fit$fit$call
    Output
      bagging.data.frame(formula = Surv(time, event) ~ ., data = data, 
          weights = weights, cp = ~0, minsplit = ~2)

# boost_tree - xgboost case weights

    Code
      print(wt_fit$fit$call)
    Output
      xgboost::xgb.train(params = list(eta = 0.3, max_depth = 6, gamma = 0, 
          colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
          subsample = 1), data = x$data, nrounds = 15, watchlist = x$watchlist, 
          verbose = 0, nthread = 1, objective = "binary:logistic")

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

# LDA - sda case weights

    Code
      wt_fit$fit$call
    Output
      mda::fda(formula = Class ~ ., data = data, weights = weights, 
          method = mda::gen.ridge, keep.fitted = FALSE, lambda = ~1e-04)

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

# mlp - nnet case weights

    Case weights are not enabled by the underlying model implementation.

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
          weights = weights, lambda = ~10^(-4:-1))

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

# proportional_hazards - glmnet censored case weights

    Code
      wt_fit$fit$fit$call
    Output
      glmnet::glmnet(x = data_obj$x, y = data_obj$y, family = "cox", 
          weights = weights, alpha = alpha, lambda = lambda)

# rand_forest - ranger case weights

    Code
      print(wt_fit$fit$call)
    Output
      ranger::ranger(x = maybe_data_frame(x), y = y, num.threads = 1, 
          verbose = FALSE, seed = sample.int(10^5, 1), probability = TRUE, 
          case.weights = weights)

# survival_reg - flexsurv censored case weights

    Code
      wt_fit$fit$call
    Output
      flexsurv::flexsurvreg(formula = Surv(time, event) ~ released_theaters + 
          rated, data = data, weights = weights, dist = "weibull")

