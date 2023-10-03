library("magrittr")
library("data.table")
library("ggplot2")
library("ggpmisc")
library("gridExtra")

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
eu_growth <- merge(eu_growth, tl_coef, index = reporter)
eu_growth[, forecasts := fgrowth * tlr]
eu_growth <- eu_growth[, .(reporter, year, growth, forecasts, dif = (growth - forecasts), dif_abs = abs(growth - forecasts))]


############ TESTS ON FINAL MODEL BASED ON MCGREGOR AND SWALES (1985)
model <- plm::plm(growth ~ forecasts, data = eu_growth, model = "pooling")
residuals <- model$residuals
eu_growth <- cbind(eu_growth, residuals) ## table already contains panel of forecasts and actual annaul growth rates
data_scatter <- eu_growth[, .(growth, forecasts)] %>% as.data.frame()


############# REMOVE GREECE FOR COMPARISONS
eu_growth2 <- eu_growth[reporter != "Greece"]
model2 <- lm(growth ~ forecasts, data = eu_growth2)
residuals2 <- model2$residuals
eu_growth2 <- cbind(eu_growth2, residuals2)
data_scatter2 <- eu_growth2[, .(growth, forecasts)] %>% as.data.frame()
#############
par(mfrow = c(2, 1))

plot1 <- ggplot(data_scatter, aes(x = forecasts, y = growth)) +
    geom_point() +
    stat_poly_line(method = lm, se = FALSE, color = "black") +
    # stat_poly_eq() +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2) +
    ggtitle("Δy = -0.002 + 1.093MSTL       R^2: 0.532", subtitle = "Actual growth vs MSTL. Dashed line defines 45% rule.")

plot2 <-
    ggplot(data_scatter2, aes(x = forecasts, y = growth)) +
    geom_point() +
    stat_poly_line(method = lm, se = FALSE, color = "black") +
    # stat_poly_eq() +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2) +
    ggtitle("Δy = -0.001 + 1.051MSTL       R^2: 0.682", subtitle = "Actual growth vs MSTL. Dashed line defines 45% rule. Greece excluded.")

grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

par(mfrow = c(1, 1))

plot(c(1:216), data_scatter2$growth, type = "l", col = "blue", ylab = "", xlab = "", axes = F)
par(new = T)
plot(c(1:216), data_scatter2$forecasts, type = "l", col = "red", ylab = "", xlab = "Actual vs Forecasted growth rates")
legend(
    x = "bottomright",
    legend = c("Y", "Y forec."),
    fill = c("blue", "red"),
    box.lty = 0,
    text.font = 1,
    bg = "transparent"
)

model <- plm::plm(growth ~ forecasts, data = eu_growth, model = "pooling")
residuals <- model$residuals
eu_growth <- cbind(eu_growth, residuals) ## table already contains panel of forecasts and actual annaul growth rates
summary(model) %>% print() ## summary coefficients, t tests, RMSE, R^2 and joint F-test
plm::purtest(residuals ~ 1, data = eu_growth, index = c("reporter", "year")) %>% print() ## panel stationarity
lmtest::bptest(model) %>% print() ## breusch-pagan homocedasticity
plm::pwtest(model) %>% print() ## wooldridge unobserved effects
Metrics::mae(eu_growth$growth, eu_growth$forecasts) %>% print() ## mean absolute error
