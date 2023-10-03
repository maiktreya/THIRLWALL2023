[//]: <> (This is also a comment.)

FINAL CONSIDERATIONS ON PANEL OF sectors SYSTEM MODEL
========================================================================================================================

---

---

method = "SUR" for panel of sectors
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices   + lag_tech_imp  + dif_income  + dif_rprices"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices   + lag_tech_exp  + dif_fincome + dif_rprices"
control_system <- systemfit.control( maxiter = 200)
```

- method = "SUR"  Result: 0.04555719 (0.04123278 with dummies)

---

---

method = "3SLS" for panel of sectors no lag dep both EXP IMP
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices     + dif_income  + dif_rprices"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices     + dif_fincome + dif_rprices"
control_system <- systemfit.control(  maxiter = 200)
```

- method = "SUR"  Result: 0.04368423 (0.04055478 with dummies)
