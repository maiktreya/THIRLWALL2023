library(magrittr)
library(data.table)
library(comtradr)


set_primary_comtrade_key("a22118ffda8c47339b31151f2d935095")

lista_paises <- ct_get_ref_table("reporter") %>% as.data.table()
clas_sitc4 <- ct_get_ref_table("S4") %>% as.data.table()

# Query goods data for China's trade with Argentina and Germany in 2019
active <- TRUE
if (active) {
    get_trade_data <- function(reporters, commodity_codes) {
        Sys.sleep(1)
        ct_get_data(
            type = "goods",
            commodity_classification = "HS",
            commodity_code = commodity_codes,
            reporter = reporters,
            partner = c("ARG", "DEU"),
            start_date = "2019",
            end_date = "2019",
            flow_direction = "Import",
            partner_2 = "World",
            verbose = TRUE
        )
    }
}

example <- get_trade_data("CHN", "TOTAL") %>% as.data.table()


example_sum <- example %>%
    .[, .(sum = sum(as.numeric(primary_value), na.rm = TRUE)),
        by = .(period, reporter_code)
    ]

print(example)
print(example_sum)
