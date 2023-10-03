[//]: <> (This is also a comment.)

FINAL CONSIDERATIONS ON PANEL OF countries SYSTEM MODEL
========================================================================================================================

---

---

method = "SUR" for panel of countries
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices   + lag_tech_imp  + dif_income  + dif_rprices"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices   + lag_tech_exp  + dif_fincome + dif_rprices"
control_system <- systemfit.control( maxiter = UNITERATED)
```

- method = "SUR" IV Result: 0.04206415 (0.04060136 with dummies)

---

---

method = "3SLS" for panel of countries no lag dep both EXP IMP
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices     + dif_income  + dif_rprices"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices     + dif_fincome + dif_rprices"
control_system <- systemfit.control(  maxiter = UNITERATED)
```

- method = "SUR" : 0.05798346  (0.05903436 with dummies)
