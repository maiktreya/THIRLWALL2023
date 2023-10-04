# Ecm for systemfit

library(magrittr)

##################################################
uecm_systemfit <- function(
    col_names = c(),
    nlags = 1,
    method = "SUR",
    method_solv = "EViews",
    iterations = 1,
    dt = data.table::data.table()) {
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
    dt <- plm::pdata.frame(dt, index = c("reporter", "year"))
    lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system)
    return(lm_result)
}

#################################################
get_ect_systemfit <- function(systemfit_uecm_coefs, sel_variables) {
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

##################################################
recm_systemfit <- function(
    col_names = c(),
    uecm_model,
    method = "SUR",
    method_solv = "EViews",
    iterations = 1,
    nlags = 1,
    dt = data.table::data.table()) {
    diff_cols <- c()
    all_lag_cols <- c()

    # get and incorporate ECT from UECM
    ect_test <- get_ect_systemfit(
        systemfit_uecm_coefs = uecm_model,
        sel_variables = col_names
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
                dt[, (lag_col) := shift(get(col), n = lag, type = "lag")]
                all_lag_cols <- c(all_lag_cols, lag_col)
            }
        }
    }

    # Construct formula string
    ifelse(nlags >= 2,
        formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], ect, all_lag_cols), collapse = " + ")),
        formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], ect), collapse = " + "))
    )

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
    dt <- plm::pdata.frame(dt, index = c("reporter", "year"))
    lm_result <- systemfit::systemfit(as.formula(formula_str), data = dt, method = method, control = control_system)
    return(lm_result)
}

######################################
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
# Define dataset of usage (data.table required) and selected variables for coint. analysis. The dependant var should be listed first.
table_dt <- fread("Data/.CSV/COMTRADE/eudata_final_nom.csv")[tech == "HIGH"]
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
