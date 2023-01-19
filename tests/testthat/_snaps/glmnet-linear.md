# error traps

    Code
      linear_reg(penalty = 0.01) %>% set_engine("glmnet") %>% fit(mpg ~ ., data = mtcars[
        -(1:4), ]) %>% predict(mtcars[-(1:4), ], penalty = 0:1)
    Error <rlang_error>
      `penalty` should be a single numeric value. `multi_predict()` can be used to get multiple predictions per row of data.

---

    Code
      linear_reg() %>% set_engine("glmnet") %>% fit(mpg ~ ., data = mtcars[-(1:4), ])
    Error <rlang_error>
      For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

