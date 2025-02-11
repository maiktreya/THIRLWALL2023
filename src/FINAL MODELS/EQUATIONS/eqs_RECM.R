eu_data <- fread("Data/CSV/COMTRADE/eudata_final_nom.csv")
eu_data <- eu_data[reporter %in% countries]
eu_data[, tech_imports := log(msum) - mprices / 100 - log(xrate)]
eu_data[, tech_exports := log(xsum) - xprices / 100 - log(xrate)]
eu_data[, dummy := 0]
dummy_sec <- c(2009, 2011)
dummy_sec2 <- c(2009, 2011)
eu_data[year %in% dummy_sec & reporter != "Greece", dummy := 1]
eu_data[year %in% dummy_sec2 & reporter == "Greece", dummy := 1]
imp_fun_pre <- "dif_tech_imp ~ ect_m + dif_rprices + dif_consum + dif_exp"
imp_fun_pos <- "dif_tech_imp ~ ect_m + dif_rprices"
imp_fun <- "dif_tech_imp ~ ect_m  + dif_rprices + dif_income"
inst_imp <- "            ~ ect_m  + dif_rprices + dif_consum  + dif_exp"
exp_fun <- "dif_tech_exp ~ ect_x  + dif_rprices + dif_fincome"
control_system <- systemfit.control(
    methodResidCov = "noDfCor",
    residCovWeighted = FALSE,
    maxiter = 200,
    tol = 1e-5,
    method3sls = "EViews" # GLS(default), IV, GMM, SCHMIDT, EVIEWS
)
control_system1 <- systemfit.control(
    methodResidCov = "noDfCor",
    residCovWeighted = FALSE,
    maxiter = 200,
    tol = 1e-5,
)
