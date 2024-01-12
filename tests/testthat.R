library(testthat)
library(extratests)

# triage with fewer files:
# devtools::test(filter = "(survival)|(censor)")

test_check("extratests", reporter = "summary")
