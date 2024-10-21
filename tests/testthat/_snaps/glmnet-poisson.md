# model errors on missing penalty value

    Code
      poisson_reg() %>% set_engine("glmnet") %>% fit(mpg ~ ., data = mtcars[-(1:4), ])
    Condition
      Error in `translate()`:
      x For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      ! There are 0 values for `penalty`.
      i To try multiple values for total regularization, use the tune package.
      i To predict multiple penalties, use `multi_predict()`.

# predict() errors with multiple penalty values

    Code
      poisson_reg(penalty = 0.1) %>% set_engine("glmnet") %>% fit(mpg ~ ., data = mtcars[
        -(1:4), ]) %>% predict(mtcars[-(1:4), ], penalty = 0:1)
    Condition
      Error in `predict()`:
      ! `penalty` should be a single numeric value.
      i `multi_predict()` can be used to get multiple predictions per row of data.

