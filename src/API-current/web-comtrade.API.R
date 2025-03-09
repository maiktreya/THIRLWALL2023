################################################################################################################
library("httr")
library("dplyr")
library("data.table")
library("jsonlite")

# Helper function for API calls
get_comtrade_data <- function(reporters, commodities, year, direction = "M") {
  base_url <- "https://comtradeapi.un.org/public/v1/get"

  # Convert reporters to comma-separated string
  reporters_str <- paste(reporters, collapse = ",")
  commodities_str <- paste(commodities, collapse = ",")

  # Build query URL
  url <- sprintf(
    "%s/flow/%s/period/%d/reporters/%s/partners/all/codes/%s",
    base_url, direction, year, reporters_str, commodities_str
  )

  # Add sleep to respect rate limits (1 request per second for free API)
  Sys.sleep(1.1)

  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    return(data$data)
  } else {
    warning(sprintf("API call failed with status %d", status_code(response)))
    return(NULL)
  }
}

# Load product categories
products_cat <- read.csv2("Data/CSV/comtrade.csv", sep = "\t")
group1 <- c("Austria", "Belgium", "Finland", "France", "Germany")
group2 <- c("Greece", "Italy", "Netherlands", "Portugal", "Spain")
group3 <- c("Ireland")

# Extract product codes
PRIM <- products_cat$PRIM[!is.na(products_cat$PRIM)] %>% as.character()
RES <- products_cat$RES[!is.na(products_cat$RES)] %>% as.character()
LOW <- products_cat$LOW[!is.na(products_cat$LOW)] %>% as.character()
MED <- products_cat$MED[!is.na(products_cat$MED)] %>% as.character()
HIGH <- products_cat$HIGH[!is.na(products_cat$HIGH)] %>% as.character()

active <- TRUE
if (active == TRUE) {
  # Process data for each group
  years <- 2010:2022 # Adjust year range as needed

  process_group <- function(group, codes) {
    results <- list()
    for (year in years) {
      data <- get_comtrade_data(group, codes, year)
      if (!is.null(data)) {
        dt <- data.table(
          year = year,
          reporter = data$reporterDesc,
          sum = data$primaryValue
        )
        results[[as.character(year)]] <- dt
      }
    }
    return(rbindlist(results))
  }

  # Get data for each group
  m_a <- process_group(group1, HIGH)
  m_b <- process_group(group2, HIGH)
  m_c <- process_group(group3, HIGH)

  # Combine all results
  m <- rbind(m_a, m_b, m_c)
  m <- m[, list(sum = sum(sum, na.rm = TRUE)), by = list(year, reporter)]

  # Optionally save results
  # write.csv2(m, file = "Data/CSV/COMTRADE/imports.HIGH1.csv")
}

# Read saved data if needed
primary_imports <- read.csv2("Data/CSV/COMTRADE/imports.HIGH.csv")
