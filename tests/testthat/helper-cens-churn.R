make_churn_cens_objects <- function(x) {
  suppressPackageStartupMessages(require("tidymodels"))
  suppressPackageStartupMessages(require("censored"))

  data("mlc_churn")

  mlc_churn <-
    mlc_churn %>%
    mutate(
      churned = ifelse(churn == "yes", 1, 0),
      event_time = survival::Surv(account_length, churned)
    ) %>%
    select(event_time, account_length, area_code, total_eve_calls)

  set.seed(6941)
  churn_split <- initial_split(mlc_churn)
  churn_tr <- training(churn_split)
  churn_te <- testing(churn_split)
  churn_rs <- vfold_cv(churn_tr)

  eval_times <- c(50, 100, 150)

  churn_rec <-
    recipe(event_time ~ ., data = churn_tr) %>%
    step_dummy(area_code) %>%
    step_normalize(all_predictors())

  list(
    split = churn_split,
    train = churn_tr,
    test = churn_te,
    rs = churn_rs,
    times = eval_times,
    rec = churn_rec
  )
}
