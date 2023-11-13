# .is_surv()

    Code
      parsnip:::.is_surv(1)
    Condition
      Error:
      ! The object does not have class `Surv`.

# .check_cens_type()

    Code
      parsnip:::.check_cens_type(left_c, type = "right", fail = TRUE)
    Condition
      Error:
      ! For this usage, the allowed censoring type is: 'right'

---

    Code
      parsnip:::.check_cens_type(left_c, type = c("right", "interval"), fail = TRUE)
    Condition
      Error:
      ! For this usage, the allowed censoring types are: 'right' and 'interval'

