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
          data = data, weights = weights, seed = 1, refresh = 0)

# mars - earth case weights

    Code
      print(wt_fit$fit$call)
    Output
      earth(formula = Class ~ ., data = data, weights = weights, keepxy = TRUE, 
          glm = list(family = function (link = "logit") 
          {
              linktemp <- substitute(link)
              if (!is.character(linktemp)) 
                  linktemp <- deparse(linktemp)
              okLinks <- c("logit", "probit", "cloglog", "cauchit", 
                  "log", "identity")
              family <- "binomial"
              if (linktemp %in% okLinks) 
                  stats <- make.link(linktemp)
              else if (is.character(link)) {
                  stats <- make.link(link)
                  linktemp <- link
              }
              else {
                  if (inherits(link, "link-glm")) {
                      stats <- link
                      if (!is.null(stats$name)) 
                        linktemp <- stats$name
                  }
                  else {
                      stop(gettextf("link \"%s\" not available for %s family; available links are %s", 
                        linktemp, family, paste(sQuote(okLinks), collapse = ", ")), 
                        domain = NA)
                  }
              }
              variance <- function(mu) mu * (1 - mu)
              validmu <- function(mu) all(is.finite(mu)) && all(mu > 
                  0 & mu < 1)
              dev.resids <- function(y, mu, wt) .Call(C_binomial_dev_resids, 
                  y, mu, wt)
              aic <- function(y, n, mu, wt, dev) {
                  m <- if (any(n > 1)) 
                      n
                  else wt
                  -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * 
                      y), round(m), mu, log = TRUE))
              }
              simfun <- function(object, nsim) {
                  ftd <- fitted(object)
                  n <- length(ftd)
                  ntot <- n * nsim
                  wts <- object$prior.weights
                  if (any(wts%%1 != 0)) 
                      stop("cannot simulate from non-integer prior.weights")
                  if (!is.null(m <- object$model)) {
                      y <- model.response(m)
                      if (is.factor(y)) {
                        yy <- factor(1 + rbinom(ntot, size = 1, prob = ftd), 
                          labels = levels(y))
                        split(yy, rep(seq_len(nsim), each = n))
                      }
                      else if (is.matrix(y) && ncol(y) == 2) {
                        yy <- vector("list", nsim)
                        for (i in seq_len(nsim)) {
                          Y <- rbinom(n, size = wts, prob = ftd)
                          YY <- cbind(Y, wts - Y)
                          colnames(YY) <- colnames(y)
                          yy[[i]] <- YY
                        }
                        yy
                      }
                      else rbinom(ntot, size = wts, prob = ftd)/wts
                  }
                  else rbinom(ntot, size = wts, prob = ftd)/wts
              }
              structure(list(family = family, link = linktemp, linkfun = stats$linkfun, 
                  linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
                  aic = aic, mu.eta = stats$mu.eta, initialize = binomInitialize(family), 
                  validmu = validmu, valideta = stats$valideta, simulate = simfun, 
                  dispersion = 1), class = "family")
          }))

# rand_forest - ranger case weights

    Code
      print(wt_fit$fit$call)
    Output
      ranger::ranger(x = maybe_data_frame(x), y = y, num.threads = 1, 
          verbose = FALSE, seed = sample.int(10^5, 1), probability = TRUE, 
          case.weights = weights)

