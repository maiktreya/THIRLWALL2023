# --- YOUR PROVIDED SCRIPT - UNCHANGED EXCEPT FOR ANNOTATIONS ---
library("magrittr")
library("data.table")
library("ggplot2")
library("ggpmisc")
library("gridExtra")
library("plm") # Added library load as plm() is used later

eu_data <- fread("Data/CSV/COMTRADE/eudata_final_nom.csv")
total_F <- fread("Output/CSV/TL.csv")

countries <- c("Austria", "Finland", "France", "Germany", "Greece", "Italy", "Netherlands", "Portugal", "Spain")
countries_ng <- c("Austria", "Finland", "France", "Germany", "Italy", "Netherlands", "Portugal", "Spain")

tech <- c("PRIM", "RES", "LOW", "MED", "HIGH")
eu_data[, tech_imports := log(msum) - mprices / 100 - log(xrate)]
eu_data[, tech_exports := log(xsum) - xprices / 100 - log(xrate)]
eu_data <- eu_data[reporter %in% countries]


############# GET DATA TO CALCULATE MEAN GROWTH RATES ##############################################

eu_one <- eu_data[tech == "HIGH"]
eu_growth <- eu_one[, .(year, growth = c(NA, diff(income)), fgrowth = c(NA, diff(fincome))), by = reporter][year != 1992]
tl_coef <- total_F[, .(reporter, tlr = exports_c / imports_c)]
eu_growth <- merge(eu_growth, tl_coef, by = "reporter")
eu_growth[, forecasts := fgrowth * tlr]
eu_growth <- eu_growth[, .(reporter, year, growth, forecasts, dif = (growth - forecasts), dif_abs = abs(growth - forecasts))]


############ TESTS ON FINAL MODEL BASED ON MCGREGOR AND SWALES (1985)
model <- plm::plm(growth ~ forecasts, data = eu_growth, model = "pooling")
residuals <- model$residuals
eu_growth <- cbind(eu_growth, residuals) ## table already contains panel of forecasts and actual annaul growth rates
data_scatter <- eu_growth[, .(growth, forecasts)] %>% as.data.frame()


############# REMOVE GREECE FOR COMPARISONS
eu_growth2 <- eu_growth[reporter != "Greece"]
model2 <- lm(growth ~ forecasts, data = eu_growth2) # Assuming you meant lm here based on context, original had no model2 explicit fit
residuals2 <- model2$residuals
eu_growth2 <- cbind(eu_growth2, residuals2)
data_scatter2 <- eu_growth2[, .(growth, forecasts)] %>% as.data.frame()
#############
# par(mfrow = c(2, 1)) # This is for base R plots, not needed for ggplot/gridExtra

plot1 <- ggplot(data_scatter, aes(x = forecasts, y = growth)) +
    geom_point() +
    stat_poly_line(method = lm, se = FALSE, color = "black") +
    stat_poly_eq() +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2) +
    ggtitle("∆ y = -0.002 + 1.093MSTL      R^2: 0.532") +
    # <<< ANNOTATION START plot1 >>>
    # This theme call currently adjusts axis titles ("forecasts", "growth")
    theme(
        axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22)
        # OPTION 1: Add plot title size HERE, inside this theme() call:
        # , plot.title = element_text(size = 18)  # Add comma above and uncomment this line. Replace 18 with your size.
    ) +
    # This theme call currently adjusts axis tick numbers
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    # OPTION 2: OR, add plot title size as a NEW theme() call AFTER this line:
    theme(plot.title = element_text(size = 22)) # Uncomment this line. Replace 18 with your size.
# <<< ANNOTATION END plot1 >>>

plot2 <-
    ggplot(data_scatter2, aes(x = forecasts, y = growth)) +
    geom_point() +
    stat_poly_line(method = lm, se = FALSE, color = "black") +
    stat_poly_eq() +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2) +
    ggtitle("∆ y = -0.001 + 1.051MSTL      R^2: 0.682") +
    # <<< ANNOTATION START plot2 >>>
    # This theme call currently adjusts axis titles ("forecasts", "growth")
    theme(
        axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22)
        # OPTION 1: Add plot title size HERE, inside this theme() call:
        # , plot.title = element_text(size = 18)  # Add comma above and uncomment this line. Replace 18 with your size.
    ) +
    # This theme call currently adjusts axis tick numbers
    theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16)) +
    # OPTION 2: OR, add plot title size as a NEW theme() call AFTER this line:
    theme(plot.title = element_text(size = 22)) # Uncomment this line. Replace 18 with your size.
# <<< ANNOTATION END plot2 >>>

grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

# --- END OF YOUR PROVIDED SCRIPT ---
