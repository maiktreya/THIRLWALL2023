############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS #################################################
library("magrittr")
library("data.table")
library("ARDL")
library("plm")
library("systemfit")
eu_data <- fread("Data/CSV/COMTRADE/eudata_final_nom.csv")
ect <- fread("Data/CSV/COMTRADE/eudata_final_ect.csv")
eu_data[, dummy := 0]
dummy_sec <- c(2009, 2011)
dummy_sec2 <- c(2008:2019)
eu_data[year %in% dummy_sec & reporter != "Greece", dummy := 1]
eu_data[year %in% dummy_sec2 & reporter == "Greece", dummy := 1]
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
eu_data[, tech_imports := log(msum) - mprices / 100 - log(xrate)]
eu_data[, tech_exports := log(xsum) - xprices / 100 - log(xrate)]
eu_data <- eu_data[reporter %in% countries]
eu_data <- merge(eu_data, ect)
coef_exp <- coef_imp <- pre_exp <- pre_imp <- pre_imp2 <- pre_imp3 <- pre_weak <- pos_weak <- list()
imp_fun_pre <- "dif_tech_imp ~ lag_inc  + lag_rprices + lag_tech_imp + dif_rprices + dif_consum +  dif_inv  + dif_exp"
imp_fun_pos <- "dif_tech_imp ~ lag_inc  + lag_rprices + lag_tech_imp + dif_rprices"
imp_fun <- "dif_tech_imp ~ lag_inc  + lag_rprices + lag_tech_imp  + dif_rprices + dif_income"
inst_imp <- "            ~ lag_inc  + lag_rprices + lag_tech_imp  + dif_rprices + dif_consum + dif_inv + dif_exp"
exp_fun <- "dif_tech_exp ~ lag_finc + lag_rprices + lag_tech_exp  + dif_rprices + dif_fincome"
control_system <- systemfit.control(
    methodResidCov = "noDfCor",
    residCovWeighted = FALSE,
    maxiter = 1,
    tol = 1e-5,
    method3sls = "GLS" # GLS(default), IV, GMM, SCHMIDT, EVIEWS
)
control_system1 <- systemfit.control(
    methodResidCov = "noDfCor",
    residCovWeighted = FALSE,
    maxiter = 1,
    tol = 1e-5,
)
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
    coef_imp[[i]] <- pre_imp[[i]]$coefficients
    pre_exp[[i]] <- systemfit(as.formula(exp_fun), data = eu_data_panel, method = "SUR", control = control_system1)
    coef_exp[[i]] <- pre_exp[[i]]$coefficients
}

final_object <- data.table()
for (i in tech) {
    for (r in seq_along(countries)) {
        obs_m <- data.table(summary(pre_imp[[i]])$eq[[r]]$coefficients[, c("Estimate", "Pr(>|t|)")], keep.rownames = T)[, name := paste0(i, "-imp-", countries[r])]
        obs_x <- data.table(summary(pre_exp[[i]])$eq[[r]]$coefficients[, c("Estimate", "Pr(>|t|)")], keep.rownames = T)[, name := paste0(i, "-exp-", countries[r])]
        final_object <- rbind(final_object, obs_m, obs_x)
    }
}
colnames(final_object) <- c("var", "est", "prob", "unit")
source("src/LIBRARY/functions2023.R")

test1 <- nrow(final_object[var == "dif_fincome" & prob > 0.05, ]) / nrow(final_object[var == "dif_fincome", ])
test2 <- nrow(final_object[var == "dif_income" & prob > 0.05, ]) / nrow(final_object[var == "dif_income", ])
test3 <- nrow(final_object[var == "dif_rprices" & unit %like% "imp" & prob > 0.05, ]) / nrow(final_object[var == "dif_rprices" & unit %like% "imp", ])
test4 <- nrow(final_object[var == "dif_rprices" & unit %like% "exp" & prob > 0.05, ]) / nrow(final_object[var == "dif_rprices" & unit %like% "exp", ])

joint_test_coef <- c(test1, test2, test3, test4)
names(joint_test_coef) <- c("dif_fincome", "dif_income", "dif_rprices_m", "dif_rprices_x")
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

        pre <- summary(AER::ivreg(diff(tech_imports) ~ diff(income) + diff(rprices) + ect_m[2:28] + dummy[2:28] |
            diff(consump) + diff(investment) + diff(exports) + diff(rprices) + ect_m[2:28] + dummy[2:28], data = eu_data_unit), diagnostics = T)$diagnostics

        ivreg_table <- rbind(ivreg_table, data.table(pre)[, unit := paste0(i, "-imp-", r)][, test := c("Weak-IV", "Wu-Hausman", "Sargan")])
    }
}
colnames(ivreg_table) <- c("df1", "df2", "stat", "prob", "unit", "test")

iv_test1 <- nrow(ivreg_table[test == "Weak-IV" & prob < 0.05, ]) / nrow(ivreg_table[test == "Weak-IV", ])
iv_test2 <- nrow(ivreg_table[test == "Wu-Hausman" & prob < 0.05, ]) / nrow(ivreg_table[test == "Wu-Hausman", ])
iv_test3 <- nrow(ivreg_table[test == "Sargan" & prob > 0.05, ]) / nrow(ivreg_table[test == "Sargan", ])

joint_iv_test <- c(iv_test1, iv_test2, iv_test3)
names(joint_iv_test) <- c("Weak", "Wu", "Sarg")
rbind(joint_iv_test, joint_iv_test * 45) %>%
    t() %>%
    print()
