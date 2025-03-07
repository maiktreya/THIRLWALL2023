############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS ##########################################################
robust <- F
library("magrittr")
library("data.table")
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")

############# GET DATA TO CALCULATE MEAN GROWTH RATES ##############################################
eu_data <- fread("Data/CSV/COMTRADE/eudata_final_nom.csv")
eu_data[, tech_imports := log(msum) - mprices / 100 - log(xrate)]
eu_data[, tech_exports := log(xsum) - xprices / 100 - log(xrate)]
eu_data <- eu_data[reporter %in% countries]

#################################### GET GROWTH RATES AND PREPARE FOR MERGING ############################################################
mean_shares <- eu_data[, .(share_tech_exports = mean(share_tech_exports), share_tech_imports = mean(share_tech_imports)), by = c("tech", "reporter")]
gstart <- eu_data[year %in% c(1992), .(tech_exports, tech_imports, income, fincome, share_tech_exports, share_tech_imports), by = c("tech", "year", "reporter")]
gend <- eu_data[year %in% c(2019), .(tech_exports, tech_imports, income, fincome, share_tech_exports, share_tech_imports), by = c("tech", "year", "reporter")]
grate <- gend[, .(year, reporter, tech,
  share_tech_exports = eu_data[, .(reporter, tech, x = mean(share_tech_exports)), by = .(reporter, tech)]$x,
  share_tech_imports = eu_data[, .(reporter, tech, x = mean(share_tech_imports)), by = .(reporter, tech)]$x,
  grate_exports = (gend$tech_exports - gstart$tech_exports) / 27,
  grate_imports = (gend$tech_imports - gstart$tech_imports) / 27,
  grate_income = (gend$income - gstart$income) / 27,
  grate_fincome = (gend$fincome - gstart$fincome) / 27
)][order(reporter, tech)]

################################# IMPORT ESTIMATED ELASTICITIES / COEFFICIENTES ###################################
source("src/FINAL MODELS/pruebas.ARDL-FGLS-count.R")
coef_fincomex <- coef_exp[order(reporter, tech)]
coef_incomem <- coef_imp[order(reporter, tech)]

####################################### GENERATE SECTORAL FORECASTS ############################################################
sector_Fx <- grate[, sect_elas := share_tech_exports * coef_fincomex$coefs]
sector_Fx_sum <- sector_Fx[, sum(sect_elas), by = c("reporter")]
sector_Fm <- grate[, sect_elas := share_tech_imports * coef_incomem$coefs]
sector_Fm_sum <- sector_Fm[, sum(sect_elas), by = c("reporter")]
sector_F <- cbind(sector_Fx_sum, sector_Fm_sum$V1, sector_Fx_sum$V1 / sector_Fm_sum$V1)
colnames(sector_F) <- c("reporter", "exports_c", "imports_c", "tl_ratio")

##################################### ADD SECTORAL VALUES AND GENERATE AGGREGATE GROWTH FORECASTS #####################################
total_F <- grate[tech == "HIGH", .(reporter, grate_fincome, grate_income)]
total_F <- merge(sector_F, total_F)
total_F[, predicted_grate := exports_c * grate_fincome / imports_c]
total_F[, dif_rates := grate_income - predicted_grate]
cbind(total_F$reporter, total_F[, round(.SD, 3), .SDcols = !c("reporter", "grate_fincome")])
total_F_means <- data.table(total_F)[, reporter := NULL][, lapply(abs(.SD), mean)]
total_F <- rbind(total_F, cbind(reporter = "MEANS", total_F_means))
total_F %>% print()


elas_table <- elas_table_x <- elas_table_m <- data.frame()
if (robust == T) {
  for (i in tech) elas_table_x <- rbind(elas_table_x, coeftest(pre_exp[[i]], vcov = sandwich::vcovHAC(pre_exp[[i]], adjust = T))[dimnames(coeftest(pre_exp[[i]]))[[1]] %like% "dif_fincome", ])
  for (i in tech) elas_table_m <- rbind(elas_table_m, coeftest(pre_imp[[i]], vcov = sandwich::vcovHAC(pre_imp[[i]], adjust = T))[dimnames(coeftest(pre_imp[[i]]))[[1]] %like% "dif_income", ])
} else {
  for (i in tech) elas_table_x <- rbind(elas_table_x, coeftest(pre_exp[[i]])[dimnames(coeftest(pre_exp[[i]]))[[1]] %like% "dif_fincome", ])
  for (i in tech) elas_table_m <- rbind(elas_table_m, coeftest(pre_imp[[i]])[dimnames(coeftest(pre_imp[[i]]))[[1]] %like% "dif_income", ])
}

elas_table <- data.table(rbind(elas_table_x, elas_table_m), keep.rownames = T)
elas_result <- as.integer(as.numeric(elas_table$"Pr(>|t|)") > 0.1) %>%
  sum() %>%
  divide_by(length(countries) * length(tech) * 2)
paste0("elast not passed at 10%: ", round(elas_result, 3)) %>% print()

# source("src/FINAL MODELS/DIAG/sector.F-bound-test-count.R")
# source("src/FINAL MODELS/DIAG/sector.2sls-diag-count.R")
