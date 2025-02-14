############################## NEEDED PACKAGES, DATA AND INITIAL PARAMETERS ##########################################################
library(magrittr)
library(data.table)
library(systemfit)
library(plm)
library(systemfitECM) # Ensure your package is installed
eu_data <- fread("Data/CSV/COMTRADE/eudata_final_nom1.csv")

countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
bound_test_m <- bound_test_x <- data.table()

# Set control parameters (adjust according to your data)
method <- "3SLS"
estimation3SLS <- "EViews"
lags <- 1
iterations <- 1

for (i in tech) {
    # Subset data by technology sector
    eu_data_panel <- eu_data[tech == i]

    # Define variables and instruments based on test.R's equations
    # Example for imports model (adjust variables as needed)
    sel_variables_imp <- c("tech_imports", "rprices", "income", "consump")
    instruments_imp <- c("income", "investment", "consump") # Endogenous first

    # Define variables for exports model if different
    sel_variables_exp <- c("tech_exports", "rprices", "fincome")
    instruments_exp <- c("fincome", "investment", "consum")

    ####################### Estimate Unrestricted ECM for Imports ###################################################
    uecm_imp <- uecm_systemfit(
        col_names = sel_variables_imp,
        nlags = lags,
        grouping = "reporter",
        method = method,
        method_solv = estimation3SLS,
        iterations = iterations,
        dt = eu_data_panel,
        inst_list = instruments_imp
    )

    # Bounds F-test for Imports
    bounds_F_imp <- systemfit_boundsF_test(
        system_ecm = uecm_imp,
        units = countries,
        sel_variables = sel_variables_imp
    )

    ####################### Estimate Restricted ECM for Imports #####################################################
    recm_imp <- recm_systemfit(
        col_names = sel_variables_imp,
        uecm_model = uecm_imp,
        grouping = "reporter",
        method = method,
        method_solv = estimation3SLS,
        iterations = iterations,
        nunits = length(countries),
        nperiods = length(unique(eu_data_panel$year)),
        nlags = lags,
        dt = eu_data_panel,
        inst_list = instruments_imp
    )

    ####################### Repeat for Exports Model #################################################################
    uecm_exp <- uecm_systemfit(
        col_names = sel_variables_exp,
        nlags = lags,
        grouping = "reporter",
        method = method,
        method_solv = estimation3SLS,
        iterations = iterations,
        dt = eu_data_panel,
        inst_list = instruments_exp
    )

    # Bounds F-test for Exports
    bounds_F_exp <- systemfit_boundsF_test(
        system_ecm = uecm_exp,
        units = countries,
        sel_variables = sel_variables_exp
    )

    ####################### Store Results ###########################################################################
    # Coefficients from Restricted ECM
    coef_imp[[i]] <- recm_imp$coefficients[grepl("ect", names(recm_imp$coefficients))]
    coef_exp[[i]] <- recm_exp$coefficients[grepl("ect", names(recm_exp$coefficients))]

    # Aggregate Bounds Test Results
    bound_test_m <- cbind(bound_test_m, bounds_F_imp)
    bound_test_x <- cbind(bound_test_x, bounds_F_exp)
    colnames(bound_test_m)[ncol(bound_test_m)] <- paste0(i, "_m")
    colnames(bound_test_x)[ncol(bound_test_x)] <- paste0(i, "_x")
}

# Finalize Results
bound_test_m <- cbind(countries, bound_test_m)
bound_test_x <- cbind(countries, bound_test_x)
