############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS ##########################################################
library("magrittr")
library("data.table")
library("urca")
eu_data <- fread("Data/CSV/COMTRADE/eudata_final_nom.csv")
eu_data[, tech_imports := log(msum) - mprices / 100 - log(xrate)]
eu_data[, tech_exports := log(xsum) - xprices / 100 - log(xrate)]
eu_data[, dummy := 0]
dummy_sec <- c(2009, 2011)
dummy_sec2 <- c(2008:2019)
eu_data[year %in% dummy_sec & reporter != "Greece", dummy := 1]
eu_data[year %in% dummy_sec2 & reporter == "Greece", dummy := 1]
countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
eu_data <- eu_data[reporter %in% countries]
tested_table <- tested_table_sec <- data.table()

# STATIONARITY TEST BASED ON PESARAN 2007
for (i in countries) {
    eu_data_loop <- eu_data[reporter == i & tech == "HIGH"]
    selected <- eu_data_loop[, .(income, fincome, exports, imports, prices, fprices, investment, consump)]

    for (j in colnames(selected)) {
        ers_test <- ur.ers(diff(as.ts(selected[, ..j])), lag.max = 0)@teststat %>% as.numeric()
        kpss_test <- ur.kpss(diff(as.ts(selected[, ..j])))@teststat %>% as.numeric()
        ers_cv <- ur.ers(diff(as.ts(selected[, ..j])), lag.max = 0)@cval[, "5pct"] %>% as.numeric()
        kpss_cv <- ur.kpss(diff(as.ts(selected[, ..j])))@cval[, "5pct"] %>% as.numeric()
        tested_table <- rbind(tested_table, t(c(i, j, "TOTAL", ers_test, kpss_test, ers_cv, kpss_cv)))
    }
}

for (i in countries) {
    eu_data_loop <- eu_data[reporter == i]

    for (n in tech) {
        eu_data_loop2 <- eu_data_loop[tech == n]
        selected <- eu_data_loop2[, .(tech_exports, tech_imports, xprices, mprices)]

        for (j in colnames(selected)) {
            ers_test <- ur.ers(diff(as.ts(selected[, ..j])), lag.max = 0)@teststat %>% as.numeric()
            kpss_test <- ur.kpss(diff(as.ts(selected[, ..j])))@teststat %>% as.numeric()
            ers_cv <- ur.ers(diff(as.ts(selected[, ..j])), lag.max = 0)@cval[, "5pct"] %>% as.numeric()
            kpss_cv <- ur.kpss(diff(as.ts(selected[, ..j])))@cval[, "5pct"] %>% as.numeric()
            tested_table_sec <- rbind(tested_table_sec, t(c(i, j, n, ers_test, kpss_test, ers_cv, kpss_cv)))
        }
    }
}



#####
colnames(tested_table) <- colnames(tested_table_sec) <- c("reporter", "variable", "tech", "ers", "kpss", "ers_cv", "kpss_cv")

tested_table[kpss > kpss_cv] %>%
    nrow() %>%
    paste0(., " of 126") %>%
    print()
tested_table[kpss > kpss_cv] %>% print()
tested_table[ers < ers_cv] %>%
    nrow() %>%
    paste0(., " of 126") %>%
    print()
tested_table[ers < ers_cv] %>% print()
tested_table_sec[kpss > kpss_cv] %>%
    nrow() %>%
    paste0(., " of 180") %>%
    print()
tested_table_sec[kpss > kpss_cv] %>% print()
tested_table_sec[ers < ers_cv] %>%
    nrow() %>%
    paste0(., " of 180") %>%
    print()
tested_table_sec[ers < ers_cv] %>% print()