############################## NEEDED PACKAGES AND DATA ##########################################################
library("magrittr")
library("data.table")
library("plm")
library("systemfit")
library("aod")
if (!requireNamespace("systemfitECM", quietly = TRUE)) {
    unloadNamespace("systemfitECM")
    devtools::install_github("iliciuv/systemfitECM", force = TRUE)
}
library(systemfitECM)

# Setup parameters
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")

# Source equations and data preparation
source("src/FINAL MODELS/EQUATIONS/eqs_UECM.R")

# set empty data containers
pre_imp <- pre_exp <- coef_imp <- coef_exp <- list()
bound_test_m <- bound_test_x <- data.table()

####################### ESTIMATE MODELS FOR EACH TECHNOLOGY LEVEL ###################################################
for (i in tech) {
    eu_data_panel <- eu_data[tech == i]

    # Import equations estimation
    sel_variables_imp <- c("tech_imports", "income", "rprices")
    instruments_imp <- c("income", "consump", "exports")

    pre_imp[[i]] <- uecm_systemfit(
        dt = eu_data_panel,
        col_names = sel_variables_imp,
        nlags = 1,
        grouping = "reporter",
        method = "3SLS",
        iterations = 1,
        method_solv = "EViews",
        inst_list = instruments_imp
    )

    # Export equations estimation
    sel_variables_exp <- c("tech_exports", "fincome", "rprices")

    pre_exp[[i]] <- uecm_systemfit(
        dt = eu_data_panel,
        col_names = sel_variables_exp,
        nlags = 1,
        grouping = "reporter",
        method = "SUR",
        iterations = 1,
    )

    ##### BOUND TEST ESTIMATION
    bound_interm <- systemfit_boundsF_test(
        system_ecm = pre_imp[[i]],
        units = countries,
        sel_variables = sel_variables_imp
    )
    bound_interx <- systemfit_boundsF_test(
        system_ecm = pre_exp[[i]],
        units = countries,
        sel_variables = c(1, 1, 1)
    )
    bound_test_m <- cbind(bound_test_m, bound_interm)
    bound_test_x <- cbind(bound_test_x, bound_interx)
    colnames(bound_test_m)[ncol(bound_test_m)] <- paste0(i, "_m")
    colnames(bound_test_x)[ncol(bound_test_x)] <- paste0(i, "_x")

    # coefficient extraction
    coef_imp[[i]] <- pre_imp[[i]]$coefficients[names(pre_imp[[i]]$coefficients) %like% "income_diff"]
    coef_exp[[i]] <- pre_exp[[i]]$coefficients[names(pre_exp[[i]]$coefficients) %like% "fincome_diff"]
}

# Tidy bounds table
bound_test_m <- cbind(countries, bound_test_m)
bound_test_x <- cbind(countries, bound_test_x)

# Transform coef_imp and coef_exp following the approach used in test_old.R
coef_exp <- setDT(coef_exp)[, reporter := countries]
coef_imp <- setDT(coef_imp)[, reporter := countries]
coef_imp <- melt.data.table(coef_imp, id.vars = "reporter")
coef_exp <- melt.data.table(coef_exp, id.vars = "reporter")
colnames(coef_exp) <- colnames(coef_imp) <- c("reporter", "tech", "coefs")
coef_exp <- coef_exp[, .(reporter, tech, tech1 = tolower(tech), coefs)][order(reporter, tech1)][, .(reporter, tech = toupper(tech1), coefs)]
coef_imp <- coef_imp[, .(reporter, tech, tech1 = tolower(tech), coefs)][order(reporter, tech1)][, .(reporter, tech = toupper(tech1), coefs)]
bound_test_m <- as.data.table(bound_test_m)
bound_test_x <- as.data.table(bound_test_x)

bound_test_m %>% print()
bound_test_x %>% print()
