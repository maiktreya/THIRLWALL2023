################################################################################################################
source("src/LIBRARY/COMTRADE.R")
library("dplyr")
library("data.table")
products_cat <- read.csv2("Data/CSV/comtrade.csv", sep = "\t")
group1 <- c("Austria", "Belgium", "Finland", "France", "Germany")
group2 <- c("Greece", "Italy", "Netherlands", "Portugal", "Spain")
group3 <- c("Ireland")

PRIM <- products_cat$PRIM[!is.na(products_cat$PRIM)] %>% as.character()
RES <- products_cat$RES[!is.na(products_cat$RES)] %>% as.character()
LOW <- products_cat$LOW[!is.na(products_cat$LOW)] %>% as.character()
MED <- products_cat$MED[!is.na(products_cat$MED)] %>% as.character()
HIGH <- products_cat$HIGH[!is.na(products_cat$HIGH)] %>% as.character()
# 52 67 44 71 25
{
  list(PRIM[1:20], PRIM[21:40], PRIM[41:52])
  list(RES[1:20], RES[21:40], RES[41:60], RES[61:67])
  list(LOW[1:20], LOW[21:40], LOW[41:44])
  list(MED[1:20], MED[21:40], MED[41:60], MED[61:71])
  list(HIGH[1:20], HIGH[21:25])
}
active <- T
if (active == T) {
  # set.Comtrade()
  m1 <- comtradr::ct_search(
    reporters = group1,
    partners = "World",
    trade_direction = "imports",
    commod_codes = HIGH[1:20]
  )
  m1 <- data.table(m1)
  m1 <- m1[, list(sum = sum(trade_value_usd)), by = list(year, reporter)]
  m2 <- comtradr::ct_search(
    reporters = group1,
    partners = "World",
    trade_direction = "imports",
    commod_codes = HIGH[21:25]
  )
  m2 <- data.table(m2)
  m2 <- m2[, list(sum = sum(trade_value_usd)), by = list(year, reporter)]
  m_a <- rbind(m1, m2)
  m_a <- m_a[, list(sum = sum(sum)), by = list(year, reporter)]

  m11 <- comtradr::ct_search(
    reporters = group2,
    partners = "World",
    trade_direction = "imports",
    commod_codes = HIGH[1:20]
  )
  m11 <- data.table(m11)
  m11 <- m11[, list(sum = sum(trade_value_usd)), by = list(year, reporter)]
  m22 <- comtradr::ct_search(
    reporters = group2,
    partners = "World",
    trade_direction = "imports",
    commod_codes = HIGH[21:25]
  )
  m22 <- data.table(m22)
  m22 <- m22[, list(sum = sum(trade_value_usd)), by = list(year, reporter)]

  m_b <- rbind(m11, m22)
  m_b <- m_b[, list(sum = sum(sum)), by = list(year, reporter)]

  m111 <- comtradr::ct_search(
    reporters = group3,
    partners = "World",
    trade_direction = "imports",
    commod_codes = HIGH[1:20]
  )
  m111 <- data.table(m111)
  m111 <- m111[, list(sum = sum(trade_value_usd)), by = list(year, reporter)]
  m222 <- comtradr::ct_search(
    reporters = group3,
    partners = "World",
    trade_direction = "imports",
    commod_codes = HIGH[21:25]
  )
  m222 <- data.table(m222)
  m222 <- m222[, list(sum = sum(trade_value_usd)), by = list(year, reporter)]


  m_c <- rbind(m111, m222)
  m_c <- m_c[, list(sum = sum(sum)), by = list(year, reporter)]





  m <- rbind(m_a, m_b, m_c)
  # write.csv2(m, file = "Data/CSV/COMTRADE/imports.HIGH1.csv")

  # save.image("RDATA/first202022.comtrade.RData")
} else {
  load("RDATA/first202022.comtrade.RData")
}

comtradr::ct_get_remaining_hourly_queries()
primary_imports <- read.csv2("Data/CSV/COMTRADE/imports.HIGH.csv")
