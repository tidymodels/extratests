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

