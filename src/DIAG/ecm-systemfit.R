#' Unrestricted Error Correction Model for Systemfit
#' @importFrom rlang :=
#' Computes the Unrestricted Error Correction Model for Systemfit.
#'
#' @param col_names A character vector of column names.
#' @param nlags An integer specifying the number of lags.
#' @param method Character string indicating the desired estimation method.
#' @param method_solv Character string indicating the solution method. Default is "EViews".
#' @param iterations An integer indicating the number of iterations.
#' @param dt A data.table object containing the data.
#' @param inst_list List of instruments for 2SLS and 3SLS.
#'
#' @return A model result from systemfit.
#' @export
uecm_systemfit <- function(
    col_names = c(),
    nlags = 1,
    method = "SUR",
    method_solv = "EViews",
    iterations = 1,
    dt = data.table::data.table(),
    inst_list = c()) {
    diff_cols <- c()
    all_lag_cols <- c()

    for (col in col_names) {
        # Add diff column
        diff_col <- paste0(col, "_diff")
        dt[, (diff_col) := diff(c(NA, get(col)))]
        diff_cols <- c(diff_cols, diff_col)

        # Add lag columns for each lag value
        for (lag in 1:nlags) {
            lag_col <- paste0(col, "_lag", lag)
            dt[, (lag_col) := data.table::shift(get(col), n = lag, type = "lag")]
            all_lag_cols <- c(all_lag_cols, lag_col)
        }
    }

    # Construct formula string
    formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], all_lag_cols), collapse = " + "))
    # Remove rows with NA values
    dt <- dt[complete.cases(dt), ]
    dt <- plm::pdata.frame(dt, index = c("reporter", "year"))

    # Run systemfit model
    if (method == "3SLS") {
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
            method3sls = "EViews" # GLS(default), IV, GMM, SCHMIDT, EVIEWS
        )
        lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system, inst = inst_list)
    }
    if (method == "2SLS") {
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
        )
        lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system, inst = inst_list)
    }
    if (method == "SUR") {
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
        )
        lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system)
    }

    return(lm_result)
}


#' Extract Error Correction Term from Systemfit Model
#'
#' Computes the Error Correction Term from Unrestricted ECM coefficients.
#'
#' @param systemfit_uecm_coefs A list containing coefficients from UECM.
#' @param sel_variables A character vector of selected variable names.
#'
#' @return A data.table object containing the error correction term.
#' @export
get_ect_systemfit <- function(systemfit_uecm_coefs, sel_variables, table_dt) {
    coef_exp <- systemfit_uecm_coefs$coefficients

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

#' Restricted Error Correction Model for Systemfit
#'
#' Computes the Restricted Error Correction Model for Systemfit.
#'
#' @param col_names A character vector of column names.
#' @param uecm_model An object of class systemfit, representing the UECM model.
#' @param method Character string indicating the desired estimation method.
#' @param method_solv Character string indicating the solution method. Default is "EViews".
#' @param iterations An integer indicating the number of iterations.
#' @param nlags An integer specifying the number of lags.
#' @param dt A data.table object containing the data.
#' @param inst_list List of instruments for 2SLS and 3SLS.
#'
#' @return A model result from systemfit.
#' @export
recm_systemfit <- function(
    col_names = c(),
    uecm_model,
    method = "SUR",
    method_solv = "EViews",
    iterations = 1,
    nlags = 1,
    dt = data.table::data.table(),
    inst_list = c()) {
    diff_cols <- c()
    all_lag_cols <- c()

    # get and incorporate ECT from UECM
    ect_test <- get_ect_systemfit(
        systemfit_uecm_coefs = uecm_model,
        sel_variables = col_names,
        table_dt = dt
    )
    dt <- cbind(dt, ect_test)
    ect <- dt$ect_test


    # Add lag columns for each lag value
    for (col in col_names) {
        # Add diff column
        diff_col <- paste0(col, "_diff")
        dt[, (diff_col) := diff(c(NA, get(col)))]
        diff_cols <- c(diff_cols, diff_col)
        if (nlags >= 2) {
            # Add lag columns for each lag value
            for (lag in 2:nlags) {
                lag_col <- paste0(col, "_lag", lag)
                dt[, (lag_col) := data.table::shift(get(col), n = lag, type = "lag")]
                all_lag_cols <- c(all_lag_cols, lag_col)
            }
        }
    }

    # Construct formula string
    ifelse(nlags >= 2,
        formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], ect, all_lag_cols), collapse = " + ")),
        formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], ect), collapse = " + "))
    )
    # Remove rows with NA values
    dt <- dt[complete.cases(dt), ]
    # Run systemfit model
    dt <- plm::pdata.frame(dt, index = c("reporter", "year"))

    # Run systemfit model
    if (method == "3SLS") {
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
            method3sls = "EViews" # GLS(default), IV, GMM, SCHMIDT, EVIEWS
        )
        lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system, inst = inst_list)
    }
    if (method == "2SLS") {
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
        )
        lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system, inst = inst_list)
    }
    if (method == "SUR") {
        control_system <- systemfit::systemfit.control(
            methodResidCov = "noDfCor",
            residCovWeighted = FALSE,
            maxiter = iterations,
            tol = 1e-5,
        )
        lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system)
    }
    return(lm_result)
}


#' Bounds F-Test for Systemfit Error Correction Model
#'
#' Applies the Bounds F-Test to the system equations based on Pesaran (2001).
#'
#' @param system_ecm An object of class systemfit, representing the ECM.
#' @param units A character vector specifying the units or entities for the model.
#'
#' @return A numeric vector with F-test results for each unit.
#' @export
systemfit_boundsF_test <- function(
    system_ecm,
    units) {
    bound_interx <- c()
    for (n in seq_along(units)) {
        ##### BOUND TEST ESTIMATION
        bound_interx[n] <- aod::wald.test(b = coef(pre_exp$eq[[n]]), Sigma = vcov(pre_exp$eq[[n]]), Terms = 2:4)$result$chi2[1] / 3
    }

    return(bound_interx)
}


##################################################
# Example usage:

library(data.table) # for simple and performant data manipulation
library(plm) # needed for systemfit to handle panel structure
library(systemfit) # for FGLS system linear models
library(magrittr) # For piping with %<% without dplyr dependencies
library(aod) # for performing F Bounds test

# Define dataset of usage (data.table required) and selected variables for coint. analysis. The dependant var should be listed first.
table_dt <- data.table::fread("Data/CSV/COMTRADE/eudata_final_nom.csv")[tech == "HIGH"]
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
sel_variables <- c("tech_exports", "rprices", "fincome")
lags <- 2
iterations <- 1

# Get an Unrestricted ECM using systemfit methods
pre_exp <- uecm_systemfit(
    dt = table_dt,
    col_names = sel_variables,
    nlags = lags,
    method = "SUR",
    iterations = iterations,
    method_solv = "EViews" # only 3sls
)

# Get a Restricted ECM using systemfit methods
pos_exp <- recm_systemfit(
    uecm_model = pre_exp,
    dt = table_dt,
    col_names = sel_variables,
    nlags = lags,
    method = "SUR",
    iterations = iterations,
    method_solv = "EViews" # only 3sls
) %>%
    summary() %>%
    print()

# Finally, apply and F Bound-Test for equations in systems following Pesaran (2001)
bounds_F_results <- systemfit_boundsF_test(
    system_ecm = pos_exp,
    units = countries
) %>% print()
