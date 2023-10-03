
# STATIONARITY/ UNIT ROOT ANALYSIS

## STATIONARITY GLS-ADF KPSS ANALYSIS (TOTAL NON PASSED AT %5)

[1] 3 KPSS MAIN
[1] 4 ERS MAIN
[1] 5 KPSS SEC
[1] 0 ERS SEC

## STATIONARITY NON PASSED RESULTS

r$> tested_table[kpss > kpss_cv]
   reporter variable  tech               ers              kpss ers_cv
1:  Finland  exports TOTAL -3.76646506899375 0.514304851477012  -1.95
2:   Greece  rprices TOTAL -3.64884418662324 0.564376071196973  -1.95
3:    Spain  exports TOTAL -3.52927166752906 0.536525116343628  -1.95
   kpss_cv
1:   0.463
2:   0.463
3:   0.463

r$> tested_table[ers < ers_cv]
      reporter variable  tech               ers              kpss
1:      Greece   income TOTAL  -1.6808768167012 0.316894572328347
2:      Greece  consump TOTAL -1.34741992946984 0.344733853889257
3: Netherlands  consump TOTAL  -1.3773696367704 0.441618472844553
4:       Spain  consump TOTAL -1.75818080012673 0.240657991626993
   ers_cv kpss_cv
1:  -1.95   0.463
2:  -1.95   0.463
3:  -1.95   0.463
4:  -1.95   0.463
