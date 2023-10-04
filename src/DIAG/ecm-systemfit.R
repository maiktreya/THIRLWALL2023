# Ecm for systemfit

library(magrittr)
library(data.table)
library(systemfit)

##################################################
uecm_systemfit <- function(
    col_names = c(),
    nlags = 1,
    method = "SUR",
    method_solv = "EViews",
    iterations = 1,
    table = data.table::data.table()) {
    diff_cols <- c()
    all_lag_cols <- c()
    dt <- table

    for (col in col_names) {
        # Add diff column
        diff_col <- paste0(col, "_diff")
        dt[, (diff_col) := diff(c(NA, get(col)))]
        diff_cols <- c(diff_cols, diff_col)

        # Add lag columns for each lag value
        for (lag in 1:nlags) {
            lag_col <- paste0(col, "_lag", lag)
            dt[, (lag_col) := shift(get(col), n = lag, type = "lag")]
            all_lag_cols <- c(all_lag_cols, lag_col)
        }
    }

    # Construct formula string
    formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], all_lag_cols), collapse = " + "))

    # Run systemfit model
    ifelse(method == "3SLS",
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
            method3sls = "EViews" # GLS(default), IV, GMM, SCHMIDT, EVIEWS
        ),
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
        )
    )
    lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system)
    return(lm_result)
}

#################################################
get_ect_systemfit <- function(systemfit_uecm_coefs, sel_variables) {
    lags_x <- coef_exp[names(systemfit_uecm_coefs$coefficients) %like% "lag"]

    # Initialize ect_x with the first term
    ect_x <- table_dt[, get(sel_variables[1])]

    # Loop through the rest of the variables in sel_variables
    for (i in 2:length(sel_variables)) {
        term <-
            table_dt[, get(sel_variables[i])] * lags_x[names(lags_x) %like% sel_variables[i]] /
                abs(lags_x[names(lags_x) %like% sel_variables[1]])
        ect_x <- ect_x - term
    }
    transf <- data.table::data.table(ect_x)
    return(transf)
}

##################################################
recm_systemfit <- function(
    col_names = c(),
    ect,
    method = "SUR",
    method_solv = "EViews",
    iterations = 1,
    table = data.table::data.table()) {
    diff_cols <- c()
    dt <- table

    for (col in col_names) {
        # Add diff column
        diff_col <- paste0(col, "_diff")
        dt[, (diff_col) := diff(c(NA, get(col)))]
        diff_cols <- c(diff_cols, diff_col)
    }
    # Add lag columns for each lag value
    # Construct formula string
    formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], ect), collapse = " + "))
    # Run systemfit model

    ifelse(method == "3SLS",
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
            method3sls = "EViews" # GLS(default), IV, GMM, SCHMIDT, EVIEWS
        ),
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
        )
    )
    # Run systemfit model
    lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system)
    return(lm_result)
}

##################################################
# Example usage:
table_dt <- fread("Data/.CSV/COMTRADE/eudata_final_nom.csv")[reporter == "Spain" & tech == "HIGH"]
sel_variables <- c("tech_exports", "fincome", "rprices")

pre_exp <- uecm_systemfit(table = table_dt, col_names = sel_variables, nlags = 2, method = "SUR")
coef_exp <- pre_exp$coefficients

ect_test <- get_ect_systemfit(systemfit_uecm_coefs = pre_exp, sel_variables = sel_variables)

table_dt <- cbind(table_dt, ect_test)

pos_exp <- recm_systemfit(table = table_dt, col_names = sel_variables, ect = table_dt$ect_test, method = "SUR") %>%
    summary() %>%
    print()
