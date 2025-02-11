############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS ##########################################################
library("magrittr")
library("data.table")
library("systemfit")
library("plm")
library(systemfitECM) # custom library for modeling
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
coef_exp <- coef_imp <- pre_exp <- pre_exp2 <- pre_imp <- pre_imp2 <- pre_weak <- pos_weak <- list()
source("src/FINAL MODELS/EQUATIONS/eqs_UECM.R")

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
    pre_exp2[[i]] <- systemfit(as.formula(exp_fun), data = eu_data_panel, method = "OLS", control = control_system1) # ,
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

bound_test_m <- bound_test_x <- data.table()
bound_interx <- bound_interm <- c()
for (m in tech) {
    for (n in seq_along(countries)) {
        ##### BOUND TEST ESTIMATION
        bound_interm[n] <- aod::wald.test(b = coef(pre_imp[[m]]$eq[[n]]), Sigma = vcov(pre_imp[[m]]$eq[[n]]), Terms = 2:4)$result$chi2[1] / 3
        bound_interx[n] <- aod::wald.test(b = coef(pre_exp[[m]]$eq[[n]]), Sigma = vcov(pre_exp[[m]]$eq[[n]]), Terms = 2:4)$result$chi2[1] / 3
    }
    bound_test_m <- cbind(bound_test_m, bound_interm)
    bound_test_x <- cbind(bound_test_x, bound_interx)
    colnames(bound_test_m)[ncol(bound_test_m)] <- paste0(m, "_m")
    colnames(bound_test_x)[ncol(bound_test_x)] <- paste0(m, "_x")
}
bound_test_m <- cbind(countries, bound_test_m)
bound_test_x <- cbind(countries, bound_test_x)
