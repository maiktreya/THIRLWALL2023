[//]: <> (This is also a comment.)

FINAL CONSIDERATIONS ON PANEL OF countries SYSTEM MODEL
========================================================================================================================

---

---

method = "3SLS" for panel of countries
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices   + lag_tech_imp  + dif_income  + dif_rprices"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices   + lag_tech_exp  + dif_fincome + dif_rprices"
inst_imp <- "            ~ lag_consum +  lag_rprices   + lag_tech_imp  + dif_consum  + dif_rprices"
control_system <- systemfit.control(
  maxiter = 200,

)
```

- method = "3SLS" IV Result: 0.0312934 (uniterated exp and imp)
- method = "3SLS" GLS Result: 0.04442432 (uniterated exp and imp)
- method = "3SLS" Schmidt Result: 0.04900415
- method = "3SLS" Eviews Result: 0.02933175 (uniterated exp and imp)

---

---

method = "3SLS" for panel of countries with dummies
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices   + lag_tech_imp  + dif_income  + dif_rprices + dummy"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices   + lag_tech_exp  + dif_fincome + dif_rprices + dummy"
inst_imp <- "            ~ lag_consum +  lag_rprices   + lag_tech_imp  + dif_consum  + dif_rprices + dummy"
control_system <- systemfit.control(
  maxiter = 200
)
```

- method = "3SLS" IV Result: 0.03295616 (uniterated exp and imp)
- method = "3SLS" GLS Result: 0.04525895 (uniterated exp and imp)
- method = "3SLS" Schmidt Result: 0.04957134
- method = "3SLS" Eviews Result: 0.03110354 (uniterated exp and imp)

---

---

method = "3SLS" for panel of countries no lag dep both EXP IMP
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices + dif_income  + dif_rprices + dummy"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices + dif_fincome + dif_rprices + dummy"
inst_imp <- "            ~ lag_consum +  lag_rprices + dif_consum  + dif_rprices + dummy"
control_system <- systemfit.control(
  maxiter = 200
)
```

- method = "3SLS" IV Result: 0.04023409 (uniterated exp and imp)
- method = "3SLS" GLS Result: 0.1020534 (uniterated exp and imp)
- method = "3SLS" Schmidt Result: 0.02596946
- method = "3SLS" Eviews Result: 0.0348883 (uniterated exp and imp)
