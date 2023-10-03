[//]: <> (This is also a comment.)

FINAL CONSIDERATIONS ON PANEL OF SECTORS SYSTEM MODEL
========================================================================================================================

---

---

method = "3SLS" for panel of sectors
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices   + lag_tech_imp  + dif_income  + dif_rprices"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices   + lag_tech_exp  + dif_fincome + dif_rprices"
inst_imp <- "            ~ lag_consum +  lag_rprices   + lag_tech_imp  + dif_consum  + dif_rprices"
control_system <- systemfit.control(
  maxiter = 200,

)
```

- method = "3SLS" IV Result: 0.03769412
- method = "3SLS" GLS Result: 0.05057012
- method = "3SLS" Schmidt Result: 0.0483557
- method = "3SLS" Eviews Result: 0.04245059

---

---

method = "3SLS" for panel of sectors with dummies
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices   + lag_tech_imp  + dif_income  + dif_rprices + dummy"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices   + lag_tech_exp  + dif_fincome + dif_rprices + dummy"
inst_imp <- "            ~ lag_consum +  lag_rprices   + lag_tech_imp  + dif_consum  + dif_rprices + dummy"
control_system <- systemfit.control(
  maxiter = 200
)
```

- method = "3SLS" IV Result: 0.03371602 (improves to 0.0324708 if dummies 2008,2009,2011)
- method = "3SLS" GLS Result: 0.04628765
- method = "3SLS" Schmidt Result: 0.0470519
- method = "3SLS" Eviews Result: 0.03795129

---

---

method = "3SLS" for panel of sectors no lag dep both EXP IMP
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices + dif_income  + dif_rprices + dummy"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices + dif_fincome + dif_rprices + dummy"
inst_imp <- "            ~ lag_consum +  lag_rprices + dif_consum  + dif_rprices + dummy"
control_system <- systemfit.control(
  maxiter = 200
)
```

- method = "2SLS AND 3SLS ARE EQUAL: 0.02645894 (0.03002196 if no dum)

---

---

method = "3SLS" for panel of sectors  no lag dep IMP
------------------------------------------------------------------------------------------------------------------------

```R
imp_fun <- "dif_tech_imp ~ lag_inc    +  lag_rprices                   + dif_income  + dif_rprices + dummy"
exp_fun <- "dif_tech_exp ~ lag_finc   +  lag_rprices   + lag_tech_exp  + dif_fincome + dif_rprices + dummy"
inst_imp <- "            ~ lag_consum +  lag_rprices                   + dif_consum  + dif_rprices + dummy"
control_system <- systemfit.control(
  maxiter = 200
)
```

- method = "2SLS AND 3SLS ARE EQUAL: 0.02986455   (0.03445659 if no dum)
