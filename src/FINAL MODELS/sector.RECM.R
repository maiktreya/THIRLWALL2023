############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS ##########################################################
source("src/FINAL MODELS/EQUATIONS/eqs_RECM.R")

source("src/LIBRARY/functions2023.R")
ect <- fread("Data/.CSV/COMTRADE/eudata_final_ect.csv")
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
eu_data <- merge(eu_data, ect)
coef_exp <- coef_imp <- pre_exp <- pre_imp <- pre_imp2 <- pre_imp3 <- pre_weak <- pos_weak <- list()

####################### ESTIMATE ARDL WITH COMMON LONG RUN FOR IMPORTS AND EXPORTS ###################################################
for (i in tech) {
  eu_data_panel <- eu_data[tech == i]
  eu_data_panel <- pdata.frame(eu_data_panel, index = c("reporter", "year"))
  eu_data_panel$dif_rprices <- diff(eu_data_panel$xprices - eu_data_panel$mprices)
  eu_data_panel$dif_income <- diff(eu_data_panel$income)
  eu_data_panel$dif_consum <- diff(eu_data_panel$consump)
  eu_data_panel$dif_fincome <- diff(eu_data_panel$fincome)
  eu_data_panel$dif_tech_exp <- diff(eu_data_panel$tech_exports)
  eu_data_panel$dif_tech_imp <- diff(eu_data_panel$tech_imports)
  eu_data_panel$dif_inv <- diff(eu_data_panel$investment)
  eu_data_panel$dif_exp <- diff(eu_data_panel$exports)
  eu_data_panel$lag_rprices <- lag(eu_data_panel$xprices - eu_data_panel$mprices)
  eu_data_panel$lag_inc <- lag(eu_data_panel$income)
  eu_data_panel$lag_finc <- lag(eu_data_panel$fincome)
  eu_data_panel$lag_consum <- lag(eu_data_panel$consump)
  eu_data_panel$lag_tech_exp <- lag(eu_data_panel$tech_exports)
  eu_data_panel$lag_tech_imp <- lag(eu_data_panel$tech_imports)
  eu_data_panel$lag_inv <- lag(eu_data_panel$investment)
  eu_data_panel$lag_exp <- lag(eu_data_panel$exports)

  pre_imp[[i]] <- systemfit(as.formula(imp_fun), data = eu_data_panel, inst = as.formula(inst_imp), method = "3SLS", control = control_system) # ,
  pre_imp2[[i]] <- systemfit(as.formula(imp_fun), data = eu_data_panel, inst = as.formula(inst_imp), method = "2SLS", control = control_system) # ,
  pre_imp3[[i]] <- systemfit(as.formula(imp_fun), data = eu_data_panel, method = "OLS", control = control_system) # ,
  pre_weak[[i]] <- systemfit(as.formula(imp_fun_pre), data = eu_data_panel, method = "OLS", control = control_system) # ,
  pos_weak[[i]] <- systemfit(as.formula(imp_fun_pos), data = eu_data_panel, method = "OLS", control = control_system) # ,
  coef_imp[[i]] <- pre_imp[[i]]$coefficients[names(pre_imp[[i]]$coefficients) %like% "dif_income"]
  pre_exp[[i]] <- systemfit(as.formula(exp_fun), data = eu_data_panel, method = "SUR", control = control_system1)
  coef_exp[[i]] <- pre_exp[[i]]$coefficients[names(pre_exp[[i]]$coefficients) %like% "dif_fincome"]
}
coef_exp <- setDT(coef_exp)[, reporter := countries]
coef_imp <- setDT(coef_imp)[, reporter := countries]
coef_imp <- melt.data.table(coef_imp, id.vars = "reporter")
coef_exp <- melt.data.table(coef_exp, id.vars = "reporter")
colnames(coef_exp) <- colnames(coef_imp) <- c("reporter", "tech", "coefs")
coef_exp <- coef_exp[, .(reporter, tech, tech1 = tolower(tech), coefs)][order(reporter, tech1)][, .(reporter, tech = toupper(tech1), coefs)]
coef_imp <- coef_imp[, .(reporter, tech, tech1 = tolower(tech), coefs)][order(reporter, tech1)][, .(reporter, tech = toupper(tech1), coefs)]


final_object <- data.table()
for (i in tech) {
  for (r in seq_along(countries)) {
    obs_m <- data.table(summary(pre_imp[[i]])$eq[[r]]$coefficients[, c("Estimate", "Pr(>|t|)")], keep.rownames = T)[, name := paste0(i, "-imp-", countries[r])]
    obs_x <- data.table(summary(pre_exp[[i]])$eq[[r]]$coefficients[, c("Estimate", "Pr(>|t|)")], keep.rownames = T)[, name := paste0(i, "-exp-", countries[r])]
    final_object <- rbind(final_object, obs_m, obs_x)
  }
}
colnames(final_object) <- c("var", "est", "prob", "unit")
final_object %>% print()

test1 <- nrow(final_object[var == "dif_fincome" & prob > 0.05, ]) / nrow(final_object[var == "dif_fincome", ])
test2 <- nrow(final_object[var == "dif_income" & prob > 0.05, ]) / nrow(final_object[var == "dif_income", ])
test3 <- nrow(final_object[var == "ect_m" & prob > 0.05, ]) / nrow(final_object[var == "ect_m", ])
test4 <- nrow(final_object[var == "ect_x" & prob > 0.05, ]) / nrow(final_object[var == "ect_x", ])
test5 <- nrow(final_object[var == "dif_rprices" & unit %like% "imp" & prob > 0.05, ]) / nrow(final_object[var == "dif_rprices" & unit %like% "imp", ])
test6 <- nrow(final_object[var == "dif_rprices" & unit %like% "exp" & prob > 0.05, ]) / nrow(final_object[var == "dif_rprices" & unit %like% "exp", ])

joint_test_coef <- c(test1, test2, test3, test4, test5, test6)
names(joint_test_coef) <- c("dif_fincome", "dif_income", "ect_m", "ect_x", "dif_rprices_m", "dif_rprices_x")
rbind(joint_test_coef, joint_test_coef * 45) %>%
  t() %>%
  print()


######################### ENDOGENEITY ANALYSIS
# The null in Wu-Hausman is that they are equally consistent, have to reject for 2sls
ivreg_table <- data.table()
for (i in tech) {
  eu_data_panel <- eu_data[tech == i]
  for (r in countries) {
    eu_data_unit <- eu_data_panel[reporter == r]

    pre <- summary(AER::ivreg(diff(tech_imports) ~ diff(income) + diff(rprices) + ect_m[2:28] |
      diff(consump) + diff(exports) + diff(rprices) + ect_m[2:28], data = eu_data_unit), diagnostics = T)$diagnostics

    ivreg_table <- rbind(ivreg_table, data.table(pre)[, unit := paste0(i, "-imp-", r)][, test := c("Weak-IV", "Wu-Hausman", "Sargan")])
  }
}
colnames(ivreg_table) <- c("df1", "df2", "stat", "prob", "unit", "test")

iv_test1 <- nrow(ivreg_table[test == "Weak-IV" & prob < 0.05, ]) / nrow(ivreg_table[test == "Weak-IV", ])
iv_test2 <- nrow(ivreg_table[test == "Wu-Hausman" & prob < 0.05, ]) / nrow(ivreg_table[test == "Wu-Hausman", ])
iv_test3 <- nrow(ivreg_table[test == "Sargan" & prob < 0.05, ]) / nrow(ivreg_table[test == "Sargan", ])

joint_iv_test <- c(iv_test1, iv_test2, iv_test3)
names(joint_iv_test) <- c("Weak", "Wu", "Sarg")
rbind(joint_iv_test, joint_iv_test * 45) %>%
  t() %>%
  print()



################################# DURBIN-WATSON AUTOCORRELATION ##################################
bg_test <- data.table()
for (i in tech) {
  for (r in seq_along(countries)) {
    pre_m <- systemfit.bgtest(model = pre_imp[[i]]$eq[[r]])
    pre_x <- systemfit.bgtest(model = pre_exp[[i]]$eq[[r]])
    bg_test <- rbind(bg_test, data.table(id = paste0(i, "-", countries[r]), statm = pre_m$statistic, pvalm = pre_m$p.value, statx = pre_x$statistic, pvalx = pre_x$p.value))
  }
}
################################# BREUSCH-PAGAN-HETEROSCEDASTICITY ##################################
bp_test <- data.table()
for (i in tech) {
  for (r in seq_along(countries)) {
    pre_m <- systemfit.bptest(model = pre_imp[[i]]$eq[[r]])
    pre_x <- systemfit.bptest(model = pre_exp[[i]]$eq[[r]])
    bp_test <- rbind(bp_test, data.table(id = paste0(i, "-", countries[r]), statm = pre_m$statistic, pvalm = pre_m$p.value, statx = pre_x$statistic, pvalx = pre_x$p.value))
  }
}
# get_n_bound_pass(table_input = dw_test[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), ], names_first_col = T, c_value = 0.05, is_greater = T, given_name = "DW-Test") %>% print()
# get_n_bound_pass(table_input = bp_test[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), ], names_first_col = T, c_value = 0.05, is_greater = T, given_name = "BP-Test") %>% print()



# write(bp_test, "bp_test.csv")
# write(dw_test, "dw_test.csv")
# write(ivreg_table, "ivreg_stats.csv")