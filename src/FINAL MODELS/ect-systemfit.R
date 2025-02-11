############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS #################################################
library(data.table)
source("src/FINAL MODELS/EQUATIONS/eqs_UECM.R")
coef_exp <- coef_imp <- pre_exp <- pre_imp <- pre_imp2 <- pre_imp3 <- pre_weak <- pos_weak <- list()

####################### ESTIMATE ARDL WITH COMMON LONG RUN FOR IMPORTS AND EXPORTS ###################################################
for (i in tech) {
    eu_data_panel <- eu_data[tech == i]
    eu_data_panel <- pdata.frame(eu_data_panel, index = c("reporter", "year"))
    eu_data_panel$dif_rprices <- diff(eu_data_panel$rprices)
    eu_data_panel$dif_income <- diff(eu_data_panel$income)
    eu_data_panel$dif_consum <- diff(eu_data_panel$consump)
    eu_data_panel$dif_fincome <- diff(eu_data_panel$fincome)
    eu_data_panel$dif_tech_exp <- diff(eu_data_panel$tech_exports)
    eu_data_panel$dif_tech_imp <- diff(eu_data_panel$tech_imports)
    eu_data_panel$dif_inv <- diff(eu_data_panel$investment)
    eu_data_panel$dif_exp <- diff(eu_data_panel$exports)
    eu_data_panel$lag_rprices <- lag(eu_data_panel$rprices)
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

ect_table_prem <- ect_table_prex <- data.table()
for (i in tech) {
    for (j in countries) {
        transf <- transf2 <- data.table()
        lags_x <- coef_exp[[i]][names(pre_exp[[i]]$coefficients) %like% j & names(pre_exp[[i]]$coefficients) %like% "lag"]
        fincCx <- eu_data[tech == i & reporter == j, fincome[1:27]] * lags_x[names(lags_x) %like% "finc"] / abs(lags_x[names(lags_x) %like% "tech_exp"])
        rpriCx <- eu_data[tech == i & reporter == j, rprices[1:27]] * lags_x[names(lags_x) %like% "prices"] / abs(lags_x[names(lags_x) %like% "tech_exp"])
        ect_x <- eu_data[tech == i & reporter == j, tech_exports[1:27]] - rpriCx - fincCx
        transf[, ect_x := c(NA, ect_x)][, tech := i][, reporter := j][, year := c(1992:2019)]
        ect_table_prex <- rbind(ect_table_prex, transf)

        lags_m <- coef_imp[[i]][names(pre_imp[[i]]$coefficients) %like% j & names(pre_imp[[i]]$coefficients) %like% "lag"]
        fincCm <- eu_data[tech == i & reporter == j, income[1:27]] * lags_m[names(lags_m) %like% "inc"] / abs(lags_m[names(lags_m) %like% "tech_imp"])
        rpriCm <- eu_data[tech == i & reporter == j, rprices[1:27]] * lags_m[names(lags_m) %like% "prices"] / abs(lags_m[names(lags_m) %like% "tech_imp"])
        ect_m <- eu_data[tech == i & reporter == j, tech_imports[1:27]] - rpriCm - fincCm
        transf2[, ect_m := c(NA, ect_m)][, tech := i][, reporter := j][, year := c(1992:2019)]
        ect_table_prem <- rbind(ect_table_prem, transf2)
    }
}
ect_table <- merge(ect_table_prem, ect_table_prex)

fwrite(ect_table, "Data/CSV/COMTRADE/eudata_final_ect.csv") # nolint
