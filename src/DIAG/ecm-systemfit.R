# EcmSystem <- function(
#     path = "Data/.CSV/COMTRADE/eudata_final_nom.csv",
#     nlags = 1,
#     maxiter = 200,
#     dependant = NA,
#     regressors = NA,
#     method = "2SLS" # c("2SLS, SUR; ·SLS)
#     ) {
#     data <- data.table::fread(path)
#
#     for (i in regressors) {
#         new_var <- data[, as.character(i)]
#         # Create lagged column of the regressor
#         data[, paste0("lag_", i) := new_var]
#     }
#
#     return(data)
# }
# aa <- EcmSystem(dependant = "tech_imp", regressors = c("fincome"))

dt <- fread("Data/.CSV/COMTRADE/eudata_final_nom.csv")
add_diff_lag_columns <- function(dt, col_names) {
    for (col in col_names) {
        # Add diff column
        dt[, paste0(col, "_diff") := diff(c(NA, get(col)))]

        # Add lag column
        dt[, paste0(col, "_lag") := shift(get(col), n = 1, type = "lag")]
    }
    return(dt)
}

# Example usage:
# dt <- data.table(a = c(1,2,3,4,5), b = c(5,6,7,8,9))
# new_dt <- add_diff_lag_columns(dt, c("a", "b"))
# print(new_dt)
add_diff_lag_columns(dt, c("fincome"))
