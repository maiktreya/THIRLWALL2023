############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS ##########################################################
library("magrittr")
library("data.table")
eu_data <- fread("Data/CSV/COMTRADE/eudata_final_nom.csv")
period <- c(1992:2019)
excluded <- c("Belgium", "Ireland")

cumsum_CA <- eu_data[!(reporter %in% excluded)][year %in% period, .(reporter, year, imports, exports)]
cumsum_CA[, cumCA := exports - imports]
results <- cumsum_CA[, sum(cumCA), by = .(reporter)]
colnames(results) <- c("reporter", "CA")
results[, CA := round(CA, 2)]
View(results)
