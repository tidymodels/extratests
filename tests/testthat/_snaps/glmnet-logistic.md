# multi_predict() with default or single penalty value

    Code
      multi_predict(class_fit, newdata = wa_churn[1:4, vars], type = "prob")
    Condition
      Error in `multi_predict()`:
      ! Please use `new_data` instead of `newdata`.

# error traps

    Code
      logistic_reg(penalty = 0.01) %>% set_engine("glmnet") %>% fit(Class ~ log(
        funded_amnt) + int_rate + term, data = lending_club) %>% predict(lending_club,
        penalty = 0:1)
    Condition
      Error in `.check_glmnet_penalty_predict()`:
      ! `penalty` should be a single numeric value. `multi_predict()` can be used to get multiple predictions per row of data.

---

    Code
      logistic_reg() %>% set_engine("glmnet") %>% fit(Class ~ log(funded_amnt) +
        int_rate + term, data = lending_club)
    Condition
      Error in `.check_glmnet_penalty_fit()`:
      ! For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).
      * There are 0 values for `penalty`.
      * To try multiple values for total regularization, use the tune package.
      * To predict multiple penalties, use `multi_predict()`

---

    Code
      logistic_reg(penalty = 0.01) %>% set_engine("glmnet") %>% fit(Class ~ log(
        funded_amnt) + int_rate + term, data = lending_club) %>% multi_predict(
        lending_club, type = "time")
    Condition
      Error in `check_pred_type()`:
      ! For event time predictions, the object should be a censored regression.

