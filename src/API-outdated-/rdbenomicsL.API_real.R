# LIVE API FOR AMECO
invisible(library(dplyr))
invisible(library(data.table))
invisible(library(rdbnomics))
countries <- list("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Netherlands", "Portugal", "Spain")
ccode <- c("aut", "bel", "fin", "fra", "deu", "grc", "irl", "ita", "nld", "prt", "esp")
ccode1 <- c("aut", "bel", "fin", "fra", "deu", "grc", "irl", "ita", "nld", "prt", "esp")
# nominal gdp
rpri <- rdb("AMECO", "XUNRQ", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
gdp <- rdb("AMECO", "OVGD", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020) # REAL PRICES
gdp1 <- rdb("AMECO", "UVGD", dimensions = list(geo = ccode, unit = "mrd-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020) # NOMINAL PRICES
# eurozone aggregate growth
intra_share <- rdb("AMECO", "DXGI", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
extra_share <- rdb("AMECO", "DXGE", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
eaz <- rdb("AMECO", "OVGD", dimensions = list(geo = "ea12", unit = "mrd-ecu-eur-weighted-mean-of-t-t-1-national-growth-rates-weights-t-1-current-prices-in-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
usa1 <- rdb("AMECO", "OVGD", dimensions = list(geo = "usa", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020) # or OVGD [WB/WDI/NY.GDP.MKTP.KD-1W]
jap1 <- rdb("AMECO", "OVGD", dimensions = list(geo = "jpn", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020) # or OVGD
kor1 <- rdb("AMECO", "OVGD", dimensions = list(geo = "kor", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020) # or OVGD [WB/WDI/NY.GDP.MKTP.KD-1W]
ast1 <- rdb("AMECO", "OVGD", dimensions = list(geo = "aus", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
mex1 <- rdb("AMECO", "OVGD", dimensions = list(geo = "mex", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
inf <- rdb("AMECO", "PXGS", dimensions = list(geo = ccode, unit = "ecu-eur-2015-100", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
inv <- rdb("AMECO", "OITT", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
# foreign prices
finf <- rdb("AMECO", "PMGS", dimensions = list(geo = ccode, unit = "ecu-eur-2015-100", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
finf2 <- rdb("AMECO", "APGN", dimensions = list(geo = ccode, unit = "2015-100", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
# exports
exp <- rdb("AMECO", "OXGS", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
# imports
imp <- rdb("AMECO", "OMGS", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
# exchange rate
exr <- rdb("AMECO", "XNE", dimensions = list(geo = ccode, unit = "annual-average-1-ecu-eur-units-of-national-currency", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
# privte consumption
gdc <- rdb("AMECO", "OCNT", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
acc <- rdb("AMECO", "UBCA", dimensions = list(geo = ccode, unit = "percentage-of-gross-domestic-product-at-current-prices", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
acc1 <- rdb("AMECO", "UBCA", dimensions = list(geo = ccode, unit = "mrd-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
pub_debt <- rdb("AMECO", "UDGGL", dimensions = list(geo = ccode, unit = "percentage-of-gdp-at-current-prices-excessive-deficit-procedure", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2020)
pub_debt1 <- rdb("AMECO", "UDGGL", dimensions = list(geo = ccode, unit = "mrd-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1990 & original_period < 2020)



eudata2 <- cbind(pub_debt1$original_period, pub_debt1$Country, pub_debt1$value)
colnames(eudata2) <- c("period", "target", "debt")
for (i in countries) {
    eudata3 <- as.data.frame(eudata2) %>% filter(target == i)
    eudata4 <- diff(as.numeric(eudata3$debt))
    eudata5 <- as.numeric(eudata3$debt)
    assign(paste0(rep("deb."), i), diff(eudata4))
    assign(paste0(rep("def."), i), diff(eudata5))
}


################################################
eudata1 <- cbind(
    gdp$original_period, gdp$Country, gdp$value, inf$value, finf$value, exp$value, imp$value, exr$value,
    rpri$value, gdc$value, acc$value, finf2$value, gdp1$value, acc1$value, intra_share$value, extra_share$value, inv$value
)
colnames(eudata1) <- c("period", "target", "income", "prices", "fprices", "exports", "imports", "xrate", "rprices", "consump", "cacc", "new_rprices", "nom_income", "cacc1", "intra", "extra", "investment")
for (i in countries) {
    eudata <- as.data.frame(eudata1) %>% filter(target == i)
    intra_exp <- as.numeric(eudata$intra)
    extra_exp <- as.numeric(eudata$extra)
    intra_perc <- intra_exp / (intra_exp + extra_exp)
    share <- as.numeric(eudata$income) / as.numeric(eaz$value)
    income <- log(as.numeric(as.character(eudata$income)))
    nom_income <- as.numeric(eudata$nom_income)
    usa <- as.numeric(usa1$value)
    jap <- as.numeric(jap1$value)
    mex <- as.numeric(mex1$value)
    kor <- as.numeric(kor1$value)
    ast <- as.numeric(ast1$value)
    fincome1 <- as.numeric(eaz$value) - as.numeric(as.character(eudata$income))


    fincome <- log((usa + fincome1 + jap) / 3)
    rprices <- log(as.numeric(as.character(eudata$rprices)))
    exports <- log(as.numeric(as.character(eudata$exports)))
    imports <- log(as.numeric(as.character(eudata$imports)))
    caccount <- as.numeric(as.character(eudata$cacc))
    nom_account <- as.numeric(eudata$cacc1)
    consump <- log(as.numeric(as.character(eudata$consump)))
    eudata <- cbind(income, fincome, rprices, exports, imports, consump, caccount, nom_account, nom_income)
    assign(paste0(rep("d."), i), eudata)
}


###### final log-dif data.frame of values 1991-2020
listed <- list(d.Austria, d.Belgium, d.Finland, d.France, d.Germany, d.Greece, d.Ireland, d.Italy, d.Netherlands, d.Portugal, d.Spain)
positions <- c(1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 91)
frame_listed <- data.frame(listed)

LincomeDL <- as.matrix(frame_listed[, positions])
LfincomeDL <- as.matrix(frame_listed[, positions + 1])
LrpricesDL <- as.matrix(frame_listed[, positions + 2])
LexportsDL <- as.matrix(frame_listed[, positions + 3])
LimportsDL <- as.matrix(frame_listed[, positions + 4])
LconsumpDL <- as.matrix(frame_listed[, positions + 5])
caccountDL <- as.matrix(frame_listed[, positions + 6])
nom_accountDL <- as.matrix(frame_listed[, positions + 7])
nom_income1 <- as.matrix(frame_listed[, positions + 8])
debtDL <- cbind(deb.Austria, deb.Belgium, deb.Finland, deb.France, deb.Germany, deb.Greece, deb.Ireland, deb.Italy, deb.Netherlands, deb.Portugal, deb.Spain)
defpubDL <- -cbind(def.Austria, def.Belgium, def.Finland, def.France, def.Germany, def.Greece, def.Ireland, def.Italy, def.Netherlands, def.Portugal, def.Spain)


eudata_fin <- as.data.table(eudata1)[order(target, period)]

eu <- c(
    LfincomeDL[, 1],
    LfincomeDL[, 2],
    LfincomeDL[, 3],
    LfincomeDL[, 4],
    LfincomeDL[, 5],
    LfincomeDL[, 6],
    LfincomeDL[, 7],
    LfincomeDL[, 8],
    LfincomeDL[, 9],
    LfincomeDL[, 10],
    LfincomeDL[, 11]
)
eudata_fin[, fincome := eu]
write.csv(eudata_fin, "Data/CSV/COMTRADE/eudata.csv")