# Load required libraries
library(magrittr)
library(rdbnomics)
library(data.table)


codes <- c("aut", "deu", "esp", "fin", "fra", "grc", "ita", "nld", "prt")
df <- rdb("AMECO", "UBCA", dimensions = list(geo = codes, unit = "mrd-ecu-eur")) %>% data.table()
df <- df[original_period %in% c(1992:2019), .(Country, original_period, original_value)]
results <- df[, original_value := as.numeric(original_value)][, round(sum(original_value), 2), by = .(Country)]
colnames(results) <- c("reporter", "cumCA")
fwrite(results, "aa.csv")