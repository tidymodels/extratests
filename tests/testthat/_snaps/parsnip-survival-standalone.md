# .is_surv()

    Code
      parsnip:::.is_surv(1)
    Error <rlang_error>
      The object does not have class `Surv`.

# .check_cens_type()

    Code
      parsnip:::.check_cens_type(left_c, type = "right", fail = TRUE)
    Error <rlang_error>
      For this usage, the allowed censoring type is: 'right'

---

    Code
      parsnip:::.check_cens_type(left_c, type = c("right", "interval"), fail = TRUE)
    Error <rlang_error>
      For this usage, the allowed censoring types are: 'right' and 'interval'

