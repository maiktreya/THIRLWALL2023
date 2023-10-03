#------------------FIRST CONSISTENT PACKAGE DEV 2023. AUTHOR: Miguel García Duch------------------#02-2023

#' BASED ON ANY TABLE COMPUTING P.VALUES, REQUIRES A DATA TABLE
#'
get_n_bound_pass <- function(table_input, c_value, names_first_col = F, is_greater = T, given_name) {
  #' @param table_input Data Table. Object of class data.table to analyse.
  #' @param c_value Number. Critical value of reference.
  #' @param names_first_col Logical. Table first column contains names. Default is false.
  #' @param is_greater Logical. Indicates if looking for greater or lower values. Default is true.
  #' @return A named_column containing number of successes and % over total.
  #' @examples
  #'
  if (names_first_col == T) table_input <- table_input[, !1]
  if (is_greater == T) final_sum <- table_input[, sum(I(table_input > c_value))]
  if (is_greater == F) final_sum <- table_input[, sum(I(table_input < c_value))]
  per_total <- final_sum / (nrow(table_input) * (ncol(table_input)))
  final_name <- print(paste0("Tot.Passed ", given_name)) # nolint
  return(c("n passed" = final_sum, "%.passed" = per_total))
}

# Define a new vcovHC_systemfit() function that takes a systemfit.equation object as input
vcovHC.systemfit <- function(mod, eq_num = NA, type = "HC0") {
  cov_mat <- data.table::data.table()
  for (i in seq_along(1:eq_num)) {
    # Extract the residuals from the specified equation
    e <- residuals(mod$eq[[i]])
    e <- e[2:length(e)]

    # Extract the X matrix from the specified equation
    X <- as.matrix(mod$eq[[i]]$model)
    X <- X[2:nrow(X), ]

    # Estimate the covariance matrix based on the specified type
    if (type == "HC0") {
      cov_mat1 <- t(X) %*% diag(e^2) %*% X
    } else if (type == "HC3") {
      n_obs <- nrow(X)
      h <- (n_obs / (n_obs - 2)) * sum(e^2 * (X %*% solve(t(X) %*% X) %*% t(X))^2)
      cov_mat1 <- t(X) %*% diag(e^2) %*% X / (n_obs - dim(X)[2]) * h
    } else {
      stop("Invalid vcovHC type specified.")
    }
    cov_mat <- rbind(cov_mat, cov_mat1, use.names = F)
  }
  # Return the estimated covariance matrix
  return(cov_mat)
}


#' BASED ON ANY TABLE COMPUTING P.VALUES, REQUIRES A DATA TABLE
#'
systemfit_robust <- function(system_input, type_vcov = "FALSE") {
  #' @param system_input Data Table. Object of class data.table to analyse.
  #' @param type_vcov Integer. Critical value of reference.
  #' @return A named_column containing number of successes and % over total.
  #' @examples function(table_input, c_value, names_first_col = FALSE)
  #'
  lmtest::coeftest(system_input,
    vcov = sandwich::vcovHAC(system_input, diagnostics = type_vcov)
  )
}


#########  BREUSCH PAGAN HETEROCECASTICITY
#'
systemfit.bptest <- function(model, studentize = TRUE) {
  #' @param model. An equation from an object of class systemfit.
  #' @param residuals(model). Extracting residuals.
  #' @param residuals(model). Extracting fitted values.
  #'
  # Estimating auxiliary regression
  resid_se <- residuals(model) / summary(model)$sigma # nolint
  if (studentize == TRUE) bptest_reg <- lm(resid_se^2 ~ fitted(model) + I(fitted(model)^2) - 1)
  if (studentize == FALSE) bptest_reg <- lm(residuals(model)^2 ~ fitted(model) + I(fitted(model)^2) - 1)

  # Extracting test statistic
  teststat <- length(residuals(model)) * summary(bptest_reg)$r.squared
  # Calculating p-value
  pval <- 1 - pchisq(teststat, df = 2)
  # Returning test result as a list
  return(list(statistic = teststat, parameter = 2, p.value = pval, method = "Breusch-Pagan test for systemfit"))
}

############ DURBIN WATSON AUTOCORRELATION
#'
systemfit.dwtest2 <- function(model = NULL) {
  #' @param model. An equation from an object of class systemfit.
  #' @return A X2 statistic for test with its associated p-value.
  #'
  dbtest <- list(
    statistic = car::durbinWatsonTest(
      as.numeric(na.omit(model$residuals))
    ), hypothesis =
      c(
        "Test statistic value of 0: Perfect positive autocorrelation",
        "Test statistic value 0f 2: No autocorrelation.",
        "Test statistic value of 4: Perfect negative autocorrelation."
      )
  )
  return(dbtest)
}

###################### SARGAN TEST OVERIDENTIFICATION
#'
systemfit.sargan <- function(model = NULL, dataset = NULL) {
  #' @param table_input Data Table. Object of class data.table to analyse.
  #' @param c_value Number. Critical value of reference.
  #' @param names_first_col Logical. Table first column contains names. Default is false.
  #' @param is_greater Logical. Indicates if looking for greater or lower values. Default is true.
  #' @return A named_column containing number of successes and % over total.
  aux_reg <- as.character(model$termsInst)[2]
  aux_reg <- gsub(paste0(model$eqnLabel, "_"), "", aux_reg)
  sargan_eq <- lm(as.formula(paste("model$residuals[2:length(model$residuals)]~", paste(aux_reg, collapse = "+"))), data = dataset)
  sargan_coef <- summary(sargan_eq)$r.squared * nrow(dataset)
  sargan_pval <- 1 - pchisq(sargan_coef, 1)
  sargan_test <- round(c(sargan_coef, sargan_pval), 3)
  return(sargan_test)
}


####################### DURBIN-WU-HAUSMAN TEST #########################################################
#'
systemfit.wuhausman <- function(model = NULL, dataset = NULL) {
  #' @param model. An equation from an object of class systemfit.
  #' @param residuals(model). Extracting residuals.
  #' @param residuals(model). Extracting fitted values.
  #' @return
  dependantIV <- gsub(paste0(model$eqnLabel, "_"), "", as.character(model$termsInst)[2])
  exogenous <- gsub(paste0(model$eqnLabel, "_"), "", as.character(model$terms)[3])
  dependant <- gsub(paste0(model$eqnLabel, "_"), "", as.character(model$terms)[2])
  wuhausman_eq <- lm(as.formula(paste(dependantIV, "~", exogenous, collapse = "+")), data = dataset) # nolint
  wuhausman_eq2 <- lm(as.formula(paste(dependant, "~", exogenous, "+ wuhausman_eq$residuals", collapse = "+")), data = dataset)
  wuhausman_coef <- lmtest::waldtest(wuhausman_eq2, . ~ . - wuhausman_eq$residuals)
  wuhausman_test <- round(c(wuhausman_coef[3][2, ], wuhausman_coef[4][2, ]), 3)
  return(wuhausman_test)
}

####################### WEAK INSTRUMENT TEST #########################################################
#'
systemfit.weakinst <- function(pre_model = NULL, pos_model = NULL) {
  #' @param pre_model. A first-stage equation from an object of class systemfit.
  #' @param pos_model. A second-stage equation from an object of class systemfit.
  #' @return A Wald (F) statistic for the test with its associated p-value.
  fs <- pre_model
  # null first-stage (i.e. exclude IVs):
  fn <- pos_model
  # simple F-test
  weak_inst <- lmtest::waldtest(fs, fn)
  weak_inst_test <- round(c(weak_inst[3][2, ], weak_inst[4][2, ]), 3)
  return(weak_inst_test)
}

####################### ALT HAUSMAN TEST #########################################################
#'
systemfit.hausman_alt <- function(model2S = NULL, model3S = NULL) {
  #' @param model3S. A 3SLS equation from an object of class systemfit.
  #' @param model2S. A 2SLS equation from an object of class systemfit.
  #' @return The Hausman statistic and its associated p-value.
  fm_ols <- model3S
  fm_iv <- model2S
  cf_diff <- coef(fm_iv) - coef(fm_ols)
  vc_diff <- vcov(fm_iv) - vcov(fm_ols)
  x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
  hausman_test <- pchisq(x2_diff, df = Matrix::rankMatrix(vc_diff)[1], lower.tail = FALSE)
  hausman_result <- round(c(hausman_test, x2_diff), 3)
  return(hausman_result)
}


#' Perform Durbin-Watson test on a linear regression model
#'
#' The function performs the Durbin-Watson test for autocorrelation on a given linear regression model.
#'
#' @param model A linear regression model of class "systemfit".
#' @return A list with the name of the test and its corresponding test statistic value.
#' @examples
#' systemfit_fit <- systemfit(y ~ x1 + x2, method = "SUR", data = my_data)
#' durbin_watson_test(systemfit_fit)
systemfit.dwtest1 <- function(model) {
  residuals <- residuals(model)
  dw <- sum(diff(residuals)^2, na.rm = T) / sum(residuals^2, na.rm = T)
  p_value <- 2 * pnorm(abs(dw - 2), lower.tail = FALSE)
  result <- list(statistic = dw, p.value = p_value)
  class(result) <- "htest"
  return(result)
}




#' Perform Breuch-Godfrey test on a linear regression model
#'
systemfit.bgtest <- function(model) {
  # Obtain the residuals of the fitted system
  residuals_system <- model$residuals

  # Obtain the original data used in the systemfit object
  original_data <- model$model

  # Create a new data frame with the original data and the residuals
  data_with_residuals <- cbind(original_data, residuals_system)

  # Estimate the auxiliary regression, including the lagged residuals and the original independent variables
  data_with_residuals$lresid_lag1 <- c(NA, head(data_with_residuals$residuals_system, -1))
  independent <- colnames(model$model)[1]
  auxiliary_model <- lm(as.formula(paste0("residuals_system ~ . - ", independent, " + lresid_lag1")), data = data_with_residuals)

  # Perform an F-test on the auxiliary regression to test the null hypothesis of no serial correlation
  anova_result <- anova(auxiliary_model)
  bg_result <- anova_result["lresid_lag1", c("F value", "Pr(>F)")]
  colnames(bg_result) <- c("statistic", "p.value")
  return(bg_result)
}