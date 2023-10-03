############################## NEEDED PACKAGES, DATA AND INITIAL PARAMENTERS ##########################################################
library("magrittr")
library("data.table")
library("urca")
library("plm")
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


# STATIONARITY TEST BASED ON PESARAN 2007


tested_table <- data.table()
for (i in countries) {
    eu_data_loop <- eu_data[reporter == i & tech == "HIGH"]

    ## UNIT ROOT TEST (NULL: SERIE HAS A UNIT ROOT)
    df_test <- ur.df(diff(eu_data_loop$fincome))@teststat %>% as.numeric() # AUGMENTED DICKEY-FULLER (1981)
    ers_test <- ur.ers(diff(eu_data_loop$fincome))@teststat %>% as.numeric() # ELLIOT-ROTHENBERG-TOCK (1996)
    pp_test <- ur.pp(diff(eu_data_loop$fincome))@teststat %>% as.numeric() # PHILLIPS-PERRON (1988)
    kpss_test <- ur.kpss(diff(eu_data_loop$fincome))@teststat %>% as.numeric() # KWIATKOWSKY-PHILLIPSEN-SCHMIDT-SHIN (1992)
    ## UNIT ROOT UNDATED BREAK
    za_test <- ur.za(diff(eu_data_loop$fincome))@teststat %>% as.numeric() # ZIGOTT-ANDREWS (1992)
    tested_table <- rbind(tested_table, t(c(i, df_test, ers_test, pp_test, kpss_test, za_test)))
}

## CRITICAL VALUES
df_cv <- ur.df(diff(eu_data_loop$fincome))@cval[, "5pct"] %>% as.numeric()
ers_cv <- ur.ers(diff(eu_data_loop$fincome))@cval[, "5pct"] %>% as.numeric()
# pp_cv <- ur.pp(diff(eu_data_loop$fincome))@cval[,"5pct"] %>% as.numeric()
pp_cv <- NA
kpss_cv <- ur.kpss(diff(eu_data_loop$fincome))@cval[, "5pct"] %>% as.numeric()
za_cv <- ur.za(diff(eu_data_loop$fincome))@cval[2] %>% as.numeric()

colnames(tested_table) <- c("reporter", "df", "ers", "pp", "kpss", "za")


cv_values <- data.table(df_cv, ers_cv, pp_cv, kpss_cv, za_cv)
tested_table <- tested_table[, pp := NULL]

pass_table <- tested_table[
    ,
    .(
        df = as.numeric(df) / cv_values$df_cv,
        ers = as.numeric(ers) / cv_values$ers_cv,
        kpss = as.numeric(kpss) / cv_values$kpss_cv,
        za = as.numeric(za) / cv_values$za_cv
    )
]