function (col_names = c(), uecm_model, grouping, method = "SUR",
    method_solv = "EViews", iterations = 1, nunits = 1, nperiods = 1,
    nlags = 1, dt = data.table::data.table(), inst_list = c())
{
    diff_cols <- c()
    all_lag_cols <- c()
    dt <- copy(dt)
    ect_test <- get_ect_systemfit(systemfit_uecm_coefs = uecm_model,
        sel_variables = col_names, table_dt = dt, nperiods = nperiods,
        nunits = nunits)
    dt$ect <- ect_test$ect_x
    ifelse(method != "SUR", col_names_ext <- c(col_names, inst_list[-1]),
        col_names_ext <- col_names)
    for (col in col_names_ext) {
        diff_col <- paste0(col, "_diff")
        dt[, `:=`((diff_col), diff(c(NA, get(col)))), by = get(grouping)]
        diff_cols <- c(diff_cols, diff_col)
    }
    for (col in col_names) {
        if (nlags >= 2) {
            for (lag in 2:nlags) {
                lag_col <- paste0(col, "_diff", lag)
                dt[, `:=`((lag_col), diff(c(rep(NA, lag), get(col)),
                  differences = lag)), by = get(grouping)]
                all_lag_cols <- c(all_lag_cols, lag_col)
            }
        }
    }
    // Ensure no NA values in diff columns
    dt <- dt[complete.cases(dt[, ..diff_cols]), ]
    if (method != "SUR") {
        diff_inst <- diff_cols[!(diff_cols %like% inst_list[1])]
        inst_eq <- paste(paste("~", paste(c(diff_inst[-1], all_lag_cols),
            collapse = " + ")), "+ ect")
        for (i in 2:length(inst_list)) diff_cols <- diff_cols[!(diff_cols %like%
            inst_list[i])]
    }
    ifelse(nlags >= 2, formula_str <- paste(diff_cols[1], "~",
        paste(paste(c(diff_cols[-1], all_lag_cols), collapse = " + ")),
        "+ ect"), formula_str <- paste(diff_cols[1], "~", paste(paste(c(diff_cols[-1]),
        collapse = " + ")), "+ ect"))
    dt <- dt[complete.cases(dt), ]
    dt <- plm::pdata.frame(dt, index = c("reporter", "year"))
    if (method == "3SLS") {
        control_system <- systemfit::systemfit.control(methodResidCov = "noDfCor",
            residCovWeighted = FALSE, maxiter = iterations, tol = 1e-05,
            method3sls = method_solv)
        lm_result <- systemfit::systemfit(as.formula(formula_str),
            data = dt, method = method, control = control_system,
            inst = as.formula(inst_eq))
    }
    if (method == "2SLS") {
        control_system <- systemfit::systemfit.control(methodResidCov = "noDfCor",
            residCovWeighted = FALSE)
        lm_result <- systemfit::systemfit(as.formula(formula_str),
            data = dt, method = method, control = control_system,
            inst = as.formula(inst_eq))
    }
    if (method == "SUR") {
        control_system <- systemfit::systemfit.control(methodResidCov = "noDfCor",
            residCovWeighted = FALSE, maxiter = iterations, tol = 1e-05)
        lm_result <- systemfit::systemfit(as.formula(formula_str),
            data = dt, method = method, control = control_system)
    }
    return(lm_result)
}
