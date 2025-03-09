source("src/LIBRARY/COMTRADE.R")
library(dplyr)
library(data.table)
library(comtradr)

set_primary_comtrade_key("a22118ffda8c47339b31151f2d935095")

products_cat <- read.csv2("Data/CSV/comtrade.csv", sep = "\t")

group1 <- c("Austria", "Belgium", "Finland", "France", "Germany")
group2 <- c("Greece", "Italy", "Netherlands", "Portugal", "Spain")
group3 <- c("Ireland")

PRIM <- products_cat$PRIM[!is.na(products_cat$PRIM)] %>% as.character()
RES <- products_cat$RES[!is.na(products_cat$RES)] %>% as.character()
LOW <- products_cat$LOW[!is.na(products_cat$LOW)] %>% as.character()
MED <- products_cat$MED[!is.na(products_cat$MED)] %>% as.character()
HIGH <- products_cat$HIGH[!is.na(products_cat$HIGH)] %>% as.character()

active <- TRUE
if (active) {
  get_trade_data <- function(reporters, commodity_codes) {
    Sys.sleep(1)
    ct_get_data(
      reporters = reporters,
      partners = "all",
      trade_direction = "imports",
      commod_codes = commodity_codes,
      start_date = 2018,
      end_date = 2022
    ) %>%
      as.data.table() %>%
      .[, .(sum = sum(trade_value_usd, na.rm = TRUE)),
        by = .(year, reporter)
      ]
  }

  m_a <- rbind(
    get_trade_data(group1, HIGH[1:20]),
    get_trade_data(group1, HIGH[21:25])
  )
} else {
  m <- fread("Data/CSV/COMTRADE/imports.HIGH.csv")
}
