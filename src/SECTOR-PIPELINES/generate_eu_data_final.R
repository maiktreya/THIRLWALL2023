################################################################################################################
source("src/LIBRARY/COMTRADE.R")
library("magrittr")
library("data.table")
write_true <- T
countries <- c("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
products_cat <- read.csv2("Data/CSV/comtrade.csv", sep = "\t")
tech_exports <- read.csv2("Data/CSV/COMTRADE/tech_exports.csv") %>% data.table()
tech_imports <- read.csv2("Data/CSV/COMTRADE/tech_imports.csv") %>% data.table()
eu_data <- read.csv("Data/CSV/COMTRADE/eudata.csv") %>% data.table()
inst_income <- read.csv("Data/CSV/COMTRADE/inst_income.csv") %>% data.table()
fincome_eu15 <- read.csv("Data/CSV/COMTRADE/fincome_eu15.csv") %>% data.table()
fincome_ea12 <- read.csv("Data/CSV/COMTRADE/fincome_ea12.csv") %>% data.table()
fincome_ea12 <- read.csv("Data/CSV/COMTRADE/fincome_ea12.csv") %>% data.table()
prices_alt <- read.csv("Data/CSV/COMTRADE/rprices_APGS.csv") %>% data.table()
fincome_eu15_jp_us <- read.csv("Data/CSV/COMTRADE/fincome_eu15_jp_us.csv") %>% data.table()
ex_rate <- read.csv2("Data/CSV/COMTRADE/us_euro.csv", sep = ",") %>% data.table()
ex_rate <- ex_rate[year %in% (1992:2019)]
tech_exports <- tech_exports[year %in% c(1992:2019), .(reporter, year, tech, tech_exports = log(qt), xsum = sum, xqt = qt, xprices = log(as.numeric(sum) * 100 / as.numeric(qt)))]
tech_imports <- tech_imports[year %in% c(1992:2019), .(reporter, year, tech, tech_imports = log(qt), msum = sum, mqt = qt, mprices = log(as.numeric(sum) * 100 / as.numeric(qt)))]

final_tech <- merge(tech_exports, tech_imports, by = c("reporter", "year", "tech"))
eu_data <- eu_data[target %in% countries]
eu_data_external <- eu_data[, list(
    reporter = target,
    year = period,
    income = log(as.numeric(income)),
    consump = log(as.numeric(consump)),
    imports = log(as.numeric(imports)),
    fprices = fprices,
    xrate = ex_rate$erate,
    prices = prices,
    rprices = log(as.numeric(rprices)),
    old_rprices = log(as.numeric(new_rprices)),
    exports = log(as.numeric(exports)),
    inst_income = log(as.numeric(inst_income$inst_income)),
    fincome_alt = as.numeric(fincome),
    fincome = as.numeric(fincome_eu15$fincome),
    fincome_alt3 = as.numeric(fincome_eu15_jp_us$fincome),
    fincome_alt2 = as.numeric(fincome_ea12$fincome),
    investment = log(as.numeric(investment))
)]
final_eu_data <- merge(final_tech, eu_data_external, by = c("reporter", "year"))

final_eu_data[, sum_tech_exports := sum(xsum), by = c("reporter", "year")]
final_eu_data[, sum_tech_imports := sum(msum), by = c("reporter", "year")]
final_eu_data[, share_tech_exports := xsum / sum_tech_exports, by = c("reporter", "year")]
final_eu_data[, share_tech_imports := msum / sum_tech_imports, by = c("reporter", "year")]
if (write_true == T) fwrite(final_eu_data, "Data/CSV/COMTRADE/eudata_final_nom1.csv")