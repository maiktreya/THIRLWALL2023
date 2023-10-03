################################################################################################################
source("src/LIBRARY/COMTRADE.R")

library("dplyr")
library("data.table")
products_cat <- read.csv2("Data/CSV/comtrade.csv", sep = "\t")
countries <- c("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
exports_prim <- fread(paste0("Data/CSV/COMTRADE/exports.", tech[1], ".csv"))
exports_res <- fread(paste0("Data/CSV/COMTRADE/exports.", tech[2], ".csv"))
exports_low <- fread(paste0("Data/CSV/COMTRADE/exports.", tech[3], ".csv"))
exports_mid <- fread(paste0("Data/CSV/COMTRADE/exports.", tech[4], ".csv"))
exports_high <- fread(paste0("Data/CSV/COMTRADE/exports.", tech[5], ".csv"))

imports_prim <- fread(paste0("Data/CSV/COMTRADE/imports.", tech[1], ".csv"))
imports_res <- fread(paste0("Data/CSV/COMTRADE/imports.", tech[2], ".csv"))
imports_low <- fread(paste0("Data/CSV/COMTRADE/imports.", tech[3], ".csv"))
imports_mid <- fread(paste0("Data/CSV/COMTRADE/imports.", tech[4], ".csv"))
imports_high <- fread(paste0("Data/CSV/COMTRADE/imports.", tech[5], ".csv"))

imports_prim <- imports_prim[, list(reporter, year, sum, qt, tech = "PRIM")]
imports_res <- imports_res[, list(reporter, year, sum, qt, tech = "RES")]
imports_low <- imports_low[, list(reporter, year, sum, qt, tech = "LOW")]
imports_mid <- imports_mid[, list(reporter, year, sum, qt, tech = "MED")]
imports_high <- imports_high[, list(reporter, year, sum, qt, tech = "HIGH")]

exports_prim <- exports_prim[, list(reporter, year, sum, qt, tech = "PRIM")]
exports_res <- exports_res[, list(reporter, year, sum, qt, tech = "RES")]
exports_low <- exports_low[, list(reporter, year, sum, qt, tech = "LOW")]
exports_mid <- exports_mid[, list(reporter, year, sum, qt, tech = "MED")]
exports_high <- exports_high[, list(reporter, year, sum, qt, tech = "HIGH")]

tech_imports <- rbind(imports_prim[order(reporter, year)], imports_res[order(reporter, year)], imports_low[order(reporter, year)], imports_mid[order(reporter, year)], imports_high[order(reporter, year)]) %>% data.table()
tech_exports <- rbind(exports_prim[order(reporter, year)], exports_res[order(reporter, year)], exports_low[order(reporter, year)], exports_mid[order(reporter, year)], exports_high[order(reporter, year)]) %>% data.table()



write.csv2(tech_exports, file = "Data/CSV/COMTRADE/tech_exports.csv")
write.csv2(tech_imports, file = "Data/CSV/COMTRADE/tech_imports.csv")