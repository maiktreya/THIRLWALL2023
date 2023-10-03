################################################################################################################
library("jsonlite")
library("httr")
library("data.table")
library("comtradr")
products_cat <- fread("Data/CSV/comtrade.csv")
group1 <- c("Austria", "Belgium", "Finland", "France", "Germany")
group2 <- c("Greece", "Italy", "Netherlands", "Portugal", "Spain")
group3 <- c("Ireland")

dates <- "1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019"
cmd_codes <- c(881, 874, 871, 792, 712, 542, 541, 525, 778, 776, 774, 771, 764, 761, 759, 752, 751, 718, 716)


high_exp1 <- fread("Data/CSV/NEW/high/high_exp88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
high_exp2 <- fread("Data/CSV/NEW/high/high_exp00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
high_exp3 <- fread("Data/CSV/NEW/high/high_exp10_21_S4.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
high_imp1 <- fread("Data/CSV/NEW/high/high_imp88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
high_imp2 <- fread("Data/CSV/NEW/high/high_imp00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
high_imp3 <- fread("Data/CSV/NEW/high/high_imp10_21_S4.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]

med_exp1 <- fread("Data/CSV/NEW/med/med_exp88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
med_exp2 <- fread("Data/CSV/NEW/med/med_exp00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
med_exp3 <- fread("Data/CSV/NEW/med/med_exp10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
med_imp1 <- fread("Data/CSV/NEW/med/med_imp88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
med_imp2 <- fread("Data/CSV/NEW/med/med_imp00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
med_imp3 <- fread("Data/CSV/NEW/med/med_imp10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]

low_exp1 <- fread("Data/CSV/NEW/low/exp_low88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
low_exp2 <- fread("Data/CSV/NEW/low/exp_low00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
low_exp3 <- fread("Data/CSV/NEW/low/exp_low10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
low_imp1 <- fread("Data/CSV/NEW/low/imp_low88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
low_imp2 <- fread("Data/CSV/NEW/low/imp_low00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
low_imp3 <- fread("Data/CSV/NEW/low/imp_low10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]

res_exp1 <- fread("Data/CSV/NEW/res/exp_res88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
res_exp2 <- fread("Data/CSV/NEW/res/exp_res00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
res_exp3 <- fread("Data/CSV/NEW/res/exp_res10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
res_imp1 <- fread("Data/CSV/NEW/res/imp_res88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
res_imp2 <- fread("Data/CSV/NEW/res/imp_res00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
res_imp3 <- fread("Data/CSV/NEW/res/imp_res10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]

pri_exp1 <- fread("Data/CSV/NEW/pri/exp_pri88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
pri_exp2 <- fread("Data/CSV/NEW/pri/exp_pri00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
pri_exp3 <- fread("Data/CSV/NEW/pri/exp_pri10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
pri_imp1 <- fread("Data/CSV/NEW/pri/imp_pri88_99.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
pri_imp2 <- fread("Data/CSV/NEW/pri/imp_pri00_09.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]
pri_imp3 <- fread("Data/CSV/NEW/pri/imp_pri10_21.csv")[, .(reporter = ReporterDesc, year = Period, sum = as.numeric(PrimaryValue), qt = as.numeric(NetWgt))]


new_high_x <- rbind(high_exp1, high_exp2, high_exp3)[order(by = reporter, year)]
new_high_m <- rbind(high_imp1, high_imp2, high_imp3)[order(by = reporter, year)]
new_med_x <- rbind(med_exp1, med_exp2, med_exp3)[order(by = reporter, year)]
new_med_m <- rbind(med_imp1, med_imp2, med_imp3)[order(by = reporter, year)]
new_low_x <- rbind(low_exp1, low_exp2, low_exp3)[order(by = reporter, year)]
new_low_m <- rbind(low_imp1, low_imp2, low_imp3)[order(by = reporter, year)]
new_res_x <- rbind(res_exp1, res_exp2, res_exp3)[order(by = reporter, year)]
new_res_m <- rbind(res_imp1, res_imp2, res_imp3)[order(by = reporter, year)]
new_pri_x <- rbind(pri_exp1, pri_exp2, pri_exp3)[order(by = reporter, year)]
new_pri_m <- rbind(pri_imp1, pri_imp2, pri_imp3)[order(by = reporter, year)]




fwrite(new_high_x, "Data/CSV/COMTRADE/exports.HIGH.csv")
fwrite(new_high_m, "Data/CSV/COMTRADE/imports.HIGH.csv")
fwrite(new_med_x, "Data/CSV/COMTRADE/exports.MED.csv")
fwrite(new_med_m, "Data/CSV/COMTRADE/imports.MED.csv")
fwrite(new_low_x, "Data/CSV/COMTRADE/exports.LOW.csv")
fwrite(new_low_m, "Data/CSV/COMTRADE/imports.LOW.csv")
fwrite(new_res_x, "Data/CSV/COMTRADE/exports.RES.csv")
fwrite(new_res_m, "Data/CSV/COMTRADE/imports.RES.csv")
fwrite(new_pri_x, "Data/CSV/COMTRADE/exports.PRIM.csv")
fwrite(new_pri_m, "Data/CSV/COMTRADE/imports.PRIM.csv")