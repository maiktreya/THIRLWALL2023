########## INTERACTIVE HTML WIDGETS BASED IN JAVASCRIPT LIBS

####################################################
widget_charts <- function(name, text) {
  invisible(library(magrittr))
  invisible(library(highcharter))

  wid_name <- paste0(as.character(name))
  wid_text <- paste0(as.character(text))

  return(
    hchart(mtcars, "scatter", hcaes(wt, mpg, z = drat, color = hp)) %>%
      hc_title(text = wid_name)
  )
}

####################################################
widget_table <- function(dataset = iris, length = 1) {
  invisible(library(DT))

  data_get <- dataset

  return(
    datatable(as.data.frame(data_get), options = list(pageLength = length))
  )
}

####################################################
widget_ts <- function(dataset = iris, rango = NA, name = NULL) {
  invisible(library(dplyr))
  invisible(library(dygraphs))

  wid_name <- paste0(as.character(name))
  if (is.na(rango)) {
    set_final <- as.numeric(length(dataset[, 1]))
    data_get <- data.frame(time = c(1:set_final), value = dataset)
  } else if (!is.na(rango) & length(dataset[1, ]) < 2) {
    data_range <- lubridate::ymd_hms(rango)
    data_get <- xts::xts(x = as.numeric(dataset), order.by = data_range)
  }

  if (length(dataset[1, ]) >= 2) {
    return(
      dygraph(data_get, main = wid_name) %>%
        dyAxis("y", label = "GROWTH RATE", independentTicks = TRUE) %>%
        dySeries(data_get$Pred, axis = ("y")) %>%
        dySeries(data_get$Obs, axis = ("y"), stepPlot = FALSE, fillGraph = FALSE) %>%
        dyRangeSelector()
    )
  } else {
    return(
      dygraph(data_get, main = wid_name) %>%
        dyRangeSelector()
    )
  }
}

####################################################
widget_scatter <- function(x_var = rnorm(1:10), y_var = rnorm(1:10), color_line = "red", intercept = TRUE) {
  if (intercept == FALSE) {
    equation <- "y_var ~ x_var - 1"
    regresion <- summary(lm(as.formula(equation)))
    regres <- as.character(round(regresion$coefficients[1, 1], 3))
    r_sq <- as.numeric(round(regresion$adj.r.squared, 3))
    mensaje <- paste0("y =  x", regres, " R:", r_sq)
  } else {
    equation <- "y_var ~ x_var"
    regresion <- summary(lm(as.formula(equation)))
    inter <- as.character(round(regresion$coefficients[1, 1], 3))
    regres <- as.character(round(regresion$coefficients[2, 1], 3))
    r_sq <- as.numeric(round(regresion$adj.r.squared, 3))
    mensaje <- paste0("y = ", inter, " + x", regres, " R:", r_sq)
  }
  plot(y_var ~ x_var, xlab = mensaje)
  abline(lm(y_var ~ x_var), col = color_line)
}

####################################################
widget_scatter_gg <- function(dataset = NA, x_var = dataset[, 1], y_var = dataset[, 2], modo = "include") {
  invisible(library(ggplot2))
  invisible(library(ggpmisc))
  dataset_gg <- as.data.frame(dataset)
  legend_eq <- round(summary(lm(x_var ~ y_var))$coefficients[1:2, c(1, 4)], 3)
  r_squared <- round(summary(lm(x_var ~ y_var))$r.squared, 4)

  complements <- list(
    base = geom_point(shape = 1, color = "blue"),
    include = geom_smooth(method = lm, linetype = "dashed", color = "darkred"),
    exclude = NULL,
    model1 = as.data.frame(legend_eq),
    model2 = as.data.frame(r_squared)
  )

  ggplot(dataset_gg, aes(x = x_var, y = y_var)) +
    dput(complements["base"]) +
    annotate("text", x = 0.01, y = -0.08, label = as.character(complements["model1"], parse = TRUE)) +
    annotate("text", x = 0.00, y = -0.09, label = complements["model2"], parse = FALSE) +
    dput(complements[modo])
}