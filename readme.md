# Reproduction of Results: External Imbalances and the Balance of Payments Constraint

## Overview
This repository provides the necessary scripts and instructions to reproduce the results from the paper:

**"External Imbalances and the Balance of Payments Constraint: Evidence on Multi-Sector Thirlwall’s Law for Nine Eurozone Countries (1992-2019)"**

## Usage Instructions
### Manual Computation

To manually compute the results, follow these steps:

1. **Check for stationarity of implied variables**
   Run the following script:
   ```r
   source("src/FINAL MODELS/STATIONARITY/panel_stationarity.R", encoding = "UTF-8")
   ```

2. **Pre-test endogeneity on candidate models**
   Execute the script below, which internally calls the necessary diagnostic scripts from `src/DIAG`:
   ```r
   source("src/FINAL MODELS/sector.UECM_test.R", encoding = "UTF-8")
   ```

3. **Estimate elasticities using Feasible Generalized Least Squares (3SLS and SUR)**

   - **3.1. Estimate an ARDL in Unrestricted ECM form and perform F Bounds Test (Pesaran, 2001) for X and M:**
     ```r
     source("src/FINAL MODELS/sector.UECM.R", encoding = "UTF-8")
     ```

   - **3.2. Re-estimate the model in RECM (iterate results):**
     ```r
     source("src/FINAL MODELS/sector.RECM.R", encoding = "UTF-8")
     ```

4. **Use elasticities from Step 3 to estimate MSTL:**
   ```r
   source("src/FINAL MODELS/STATIONARITY/panel_stationarity.R", encoding = "UTF-8")
   ```

5. **Test the performance of MSTL by pooling yearly predictions:**
   ```r
   source("src/FINAL MODELS/STATIONARITY/panel_stationarity.R", encoding = "UTF-8")
   ```

---

### Computation via the `systemfitECM` R Library
To simplify the process, this research includes an R library named **`systemfitECM`**, which automates many aspects of ECM computation in the context of Systems of Simultaneous Equations estimation.

Although still under development, this package allows users to reproduce the results more efficiently. For example, instead of manually running:
   ```r
   source("src/FINAL MODELS/sector.UECM.R", encoding = "UTF-8")
   ```

You can alternatively use:
   ```r
   source("src/FINAL MODELS/sector.UECM.R", encoding = "UTF-8")
   ```
Both methods yield identical results.

To install and learn more about `systemfitECM`, visit:
➡ **[GitHub Repository: systemfitECM](https://github.com/maiktreya/systemfitECM)**

---
### Notes
- Ensure that all dependencies and required R packages are installed before executing the scripts.
- The scripts should be run in the order specified to ensure correct results.
- If you encounter any issues, please check the documentation or open an issue in this repository.

---

### License
This project is shared under [License Name], allowing for academic and research use with proper attribution.

For questions or contributions, feel free to open an issue or submit a pull request.

---

### Contact
For further inquiries, please contact **Miguel García-Duch** at **miguel.garcia.duch@ucm.es**.

---
