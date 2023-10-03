
                    STATIONARITY PRELIMINARY ANALYSIS
-------------------------------------------------------------------------------
A) TOTAL ECONOMY
[1] "3 of 126" don't reject null at 5%  GLS-DF (H0 -> "The serie has a unit root")
   reporter    variable     tech       ers    ers_cv
    Finland     exports    TOTAL      -3.76   -1.95
    Greece      rprices    TOTAL      -3.64   -1.95
    Spain       exports    TOTAL      -3.52   -1.95
[2] "4 of 126" reject the null at 95%      KPSS (H0 -> "The serie is stationary")
   reporter     variable    tech       kpss     kpss_cv
    Greece      income     TOTAL       0.31   0.463
    Greece      consump    TOTAL       0.34   0.463
    Nethr.      consump    TOTAL       0.44   0.463
    Spain       consump    TOTAL       0.24   0.463

B) SECTOR EXPORTS AND IMPORTS (5 tech categories)
[1] "5 of 90" don't reject null at 5%  GLS-DF (H0 -> "The serie has a unit root")
   reporter     variable   tech       ers     ers_cv
    Finland       exports   HIGH     -2.66    -1.95
    Spain         exports   PRIM     -3.62    -1.95
    Spain        imports    RES      -4.46    -1.95
    Spain        exports    LOW      -4.11    -1.95
    Spain        exports    HIGH     -3.60    -1.95
[2] "0 of 90" reject the null at 95%      KPSS (H0 -> "The serie is stationary")
-------------------------------------------------------------------------------

 TOTALS:
    - 96.3% of the series passed GLS-DF unit root test (5%).
    - 98.1% of the series passed KPSS stationarity test (95%).
    - 100% of the 216 analysed series passed at least GLS-DF or KPSS test.

This table presents the results of a preliminary analysis of stationarity for 126 total economy series and 90 sector exports and imports series. The analysis was conducted using two tests: GLS-DF (null hypothesis: "The serie has a unit root") and KPSS (null hypothesis: "The serie is stationary").

For the total economy series, 3 out of 126 did not reject the null at 5% GLS-DF, while 4 out of 126 rejected the null at 95% KPSS. For the sector exports and imports series, 5 out of 90 did not reject the null at 5% GLS-DF, while 0 out of 90 rejected the null at 95% KPSS.

Overall, the pass rate for the unit root test at 5% was 96.3%, while the pass rate for the stationarity test at 95% was 98.1%. The total series pass rate was 100%.

\begin{table}[h!]
    \setlength{\tabcolsep}{3pt}
    \centering
    \caption{STATIONARITY PRELIMINARY ANALYSIS}
    \textbf{A) TOTAL ECONOMY}\\
    \textit{"3 of 126" don't reject null at 5\% }
    \textit{GLS-DF (H0: "The series has a unit root")}

    \begin{tabular}{|c|c|c|c|c|}
    \hline
     Country & Variable & Category & GLS-DF & KPSS \\
    \hline
    Finland & exports & TOTAL & -3.76 & -1.95 \\
    Greece & rprices & TOTAL & -3.64 & -1.95 \\
    Spain & exports & TOTAL & -3.52 & -1.95 \\
    \hline
    \end{tabular}

    \textit{"4 of 126" reject null at 95\%}
    \textit{KPSS (H0: "The series is stationary")}
    \begin{tabular}{|c|c|c|c|c|}
    \hline
    Country & Variable & Category & GLS-DF & KPSS \\
    \hline
    Greece & income & TOTAL & 0.31 & 0.463 \\
    Greece & consump & TOTAL & 0.34 & 0.463 \\
    Netherlands & consump & TOTAL & 0.44 & 0.463 \\
    Spain & consump & TOTAL & 0.24 & 0.463 \\
    \hline
    \end{tabular}

    \textbf{B) SECTOR EXPORTS AND IMPORTS (5 tech categories)}
    \textit{"5 of 90" don't reject null at 5\%}
    \textit{GLS-DF (H0: "The series has a unit root")}
    \begin{tabular}{|c|c|c|c|c|}
        \hline
        Country & Variable & Category & GLS-DF & KPSS \\
        \hline
        Finland & exports & HIGH & -2.66 & -1.95 \\
        Spain & exports & PRIM & -3.62 & -1.95 \\
        Spain & imports & RES & -4.46 & -1.95 \\
        Spain & exports & LOW & -4.11 & -1.95 \\
        Spain & exports & HIGH & -3.60 & -1.95 \\
            \hline
    \end{tabular}

    \textit{"0 of 90" reject null at 95\%}
    \textit{KPSS (H0: "The series is stationary")}
    \vspace{0.5cm}

   \begin{itemize}
       \item {96.3\% of the series passed GLS-DF unit root test (5\%).}
       \item {98.1\% of the series passed KPSS stationarity test (95\%).}
       \item {100\% of the 216 analysed series passed at least GLS-DF or KPSS test.}
   \end{itemize}

\end{table}
