
# 1.LAST EMPIRICAL TASKS

- Check included prices in formulas from Methodology Cambridge Journal of economics
<------------------------------------------------------------------------>

## . USAGE

### . 1.1.PROJECT STRUCTURE

### . 1.2. LIMITATIONS ON COMTRADE DATA AND REPLICABILITY

### . 1.3. DATA PIPLINES AND SCRIPT SEQUENCES

TESTING PROCEDURE

1. Check for stationarity of implied variables. Run:

        source("src/FINAL MODELS/STATIONARITY/panel_stationarity.R", encoding = "UTF-8")

2. Pre-test endogeneity on candidate models

        source("src/FINAL MODELS/sector.UECM_test.R", encoding = "UTF-8")
        # it internally calls the script files contained in src/DIAG

3. Estimation of elasticities using Feasible Generalized Least Squares (3SLS and SUR)

◦ 3.1. Estimate an ARDL in Unrestricted ECM form an perform F Bounds Test (Pesaran, 2001) for X and M.

        source("src/FINAL MODELS/sector.UECM.R", encoding = "UTF-8")

◦ 3.2. Re-estimate the model in RECM (iterate results)

        source("src/FINAL MODELS/sector.RECM", encoding = "UTF-8")

1. Take elasticities from step 3 to estimate MSTL.

        source("src/FINAL MODELS/STATIONARITY/panel_stationarity.R", encoding = "UTF-8")

2. Test performance of MSTL pooling yearly predictions.

        source("src/FINAL MODELS/STATIONARITY/panel_stationarity.R", encoding = "UTF-8")
<------------------------------------------------------------------------>
