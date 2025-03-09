library(comtradr)
library(curl)
# Ensure the curl package is installed
if (!requireNamespace("curl", quietly = TRUE)) {
    install.packages("curl")
}

# Set the primary COMTRADE key
Sys.setenv("COMTRADE_PRIMARY" = "a22118ffda8c47339b31151f2d935095")
set_primary_comtrade_key()

ct_get_data_with_traceback <- function(...) {
    tryCatch(
        {
            ct_get_data(...)
        },
        error = function(e) {
            traceback()
            stop(e)
        }
    )
}

ct_get_data_with_traceback(
    commodity_code = "everything",
    reporter = "CHN",
    partner = "DEU",
    start_date = "2019",
    end_date = "2019",
    flow_direction = "Import"
)
