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
library(systemfitECM) # install and import this library

# Source equations and data preparation
source("src/FINAL MODELS/EQUATIONS/eqs_UECM.R")

# Setup parameters
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
pre_imp <- pre_exp <- list()

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
    instruments_exp <- c("fincome", "investment", "exports")

    pre_exp[[i]] <- uecm_systemfit(
        dt = eu_data_panel,
        col_names = sel_variables_exp,
        nlags = 1,
        grouping = "reporter",
        method = "SUR",
        iterations = 1,
        method_solv = "EViews",
        inst_list = instruments_exp
    )

    # Perform bounds testing
    bounds_F_imp <- systemfit_boundsF_test(
        system_ecm = pre_imp[[i]],
        units = countries,
        sel_variables = sel_variables_imp
    )

    bounds_F_exp <- systemfit_boundsF_test(
        system_ecm = pre_exp[[i]],
        units = countries,
        sel_variables = sel_variables_exp
    )

    # Store results in similar format as original code
    bound_test_m <- if (exists("bound_test_m")) {
        cbind(bound_test_m, setNames(data.frame(bounds_F_imp), paste0(i, "_m")))
    } else {
        data.frame(countries, setNames(data.frame(bounds_F_imp), paste0(i, "_m")))
    }

    bound_test_x <- if (exists("bound_test_x")) {
        cbind(bound_test_x, setNames(data.frame(bounds_F_exp), paste0(i, "_x")))
    } else {
        data.frame(countries, setNames(data.frame(bounds_F_exp), paste0(i, "_x")))
    }
}

# Convert results to data.table format
coef_imp <- lapply(tech, function(i) {
    data.table(
        reporter = countries,
        tech = i,
        coefs = pre_imp[[i]]$coefficients[grep("dif_income", names(pre_imp[[i]]$coefficients))]
    )
}) %>% rbindlist()

coef_exp <- lapply(tech, function(i) {
    data.table(
        reporter = countries,
        tech = i,
        coefs = pre_exp[[i]]$coefficients[grep("dif_fincome", names(pre_exp[[i]]$coefficients))]
    )
}) %>% rbindlist()
