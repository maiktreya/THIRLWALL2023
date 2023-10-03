# Ecm for systemfit

library(magrittr)
library(data.table)
library(systemfit)

add_diff_lag_columns <- function(
    col_names = c(),
    nlags = 1,
    method = "SUR",
    path = "Data/.CSV/COMTRADE/eudata_final_nom.csv") {
    diff_cols <- c()
    all_lag_cols <- c()

    dt <- fread(path)

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
    lm_result <- systemfit(as.formula(formula_str), data = dt, method = method)
    return(lm_result)
}

# Example usage:
# dt <- data.table(a = c(1,2,3,4,5), b = c(5,6,7,8,9))
# result <- add_diff_lag_columns(c("a", "b"), nlags = 2, method = "SUR")
# summary(result)
pre_exp <- add_diff_lag_columns(c("exports", "fincome"), nlags = 2, method = "SUR")
coef_exp <- pre_exp$coefficients
pre_exp %>% summary()
coef_exp %>% print()
