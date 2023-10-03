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
    lag_cols <- c()
    dt <- fread(path)

    for (col in col_names) {
        # Add diff column
        diff_col <- paste0(col, "_diff")
        dt[, (diff_col) := diff(c(NA, get(col)))]
        diff_cols <- c(diff_cols, diff_col)

        # Add lag column
        lag_col <- paste0(col, "_lag")
        dt[, (lag_col) := shift(get(col), n = 1, type = "lag")]
        lag_cols <- c(lag_cols, lag_col)
    }

    # Construct formula string
    formula_str <- paste(diff_cols[1], "~", paste(c(diff_cols[-1], lag_cols), collapse = " + "))

    # Run linear model
    lm_result <- systemfit(as.formula(formula_str), data = dt)

    return(lm_result)
}

# Example usage:
# dt <- data.table(a = c(1,2,3,4,5), b = c(5,6,7,8,9))
# result <- add_diff_lag_columns_and_run_lm(dt, c("a", "b"))
# summary(result)

add_diff_lag_columns(c("exports", "fincome")) %>% summary()
