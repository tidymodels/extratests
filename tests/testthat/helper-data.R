library(modeldata)
library(rsample)

set.seed(392)
binary_tr <- sim_logistic(200, ~ .1 + 2 * A - 3 * B + 1 * A *B, corr = .7)
binary_rs <- vfold_cv(binary_tr)
binary_te <- sim_logistic(2, ~ .1 + 2 * A - 3 * B + 1 * A *B, corr = .7)

###

set.seed(392)
three_class_tr <-
  sim_multinomial(
    500,
    ~  -0.5    +  0.6 * abs(A),
    ~ ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, - 2),
    ~ -0.6 * A + 0.50 * B -  A * B)
three_class_rs <- vfold_cv(three_class_tr)
three_class_te <-
  sim_multinomial(
    2,
    ~  -0.5    +  0.6 * abs(A),
    ~ ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, - 2),
    ~ -0.6 * A + 0.50 * B -  A * B)

###

num_tr <- sim_regression(200)
num_rs <- vfold_cv(num_tr)
num_te <- sim_regression(2)

