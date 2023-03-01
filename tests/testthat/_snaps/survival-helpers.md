# survival helpers

    The object does not have class `Surv`.

---

    Code
      parsnip:::.check_cens_type(left_c, fail = TRUE)
    Error <rlang_error>
      For this usage, the allowed censoring type is: 'right'

---

    Code
      parsnip:::.check_cens_type(left_c, type = c("right", "interval"), fail = TRUE)
    Error <rlang_error>
      For this usage, the allowed censoring types are: 'right' and 'interval'

