library(httr)
library(jsonlite)

# Set endpoint and parameters
endpoint <- "https://api.oec.world/tesseract/data"
parameters <- list(
    cube = "trade_i_baci_a_92",
    drilldowns = "Year,HS2",
    measures = "Trade Value",
    Year = "2019",
    Origin = "bra" # Brazil's ISO-3 country code
)

# Make the API call
response <- GET(url = endpoint, query = parameters)

# Check if the request was successful
if (http_status(response)$category == "Success") {
    # Parse the JSON response
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content, flatten = TRUE)

    # Extract the data from the response
    trade_data <- data$data

    # Print the trade data
    print(trade_data)
} else {
    # Print the error message
    print(http_status(response)$message)
}


library(concordance)
library(dplyr)

# Define SITC product codes for each technology category
high_tech_sitc <- c(716, 718, 751, 752, 759, 761, 764, 771, 774, 776, 778, 525, 541, 542, 712, 792, 871, 874, 881)
medium_tech_sitc <- c(781, 782, 783, 784, 785, 266, 267, 512, 513, 533, 553, 554, 562, 571, 572, 573, 574, 575, 579, 581, 582, 583, 591, 593, 597, 598, 653, 671, 672, 679, 786, 791, 882, 711, 713, 714, 721, 722, 723, 724, 725, 726, 727, 728, 731, 733, 735, 737, 741, 742, 743, 744, 745, 746, 747, 748, 749, 762, 763, 772, 773, 775, 793, 811, 812, 813, 872, 873, 884, 885, 891) # nolint
low_tech_sitc <- c(611, 612, 613, 651, 652, 654, 655, 656, 657, 658, 659, 831, 841, 842, 843, 844, 845, 846, 848, 851, 642, 665, 666, 673, 674, 675, 676, 677, 678, 691, 692, 693, 694, 695, 696, 697, 699, 821, 893, 894, 895, 897, 898, 899) # nolint
primary_products_sitc <- c(1, 11, 12, 22, 25, 34, 36, 41, 42, 43, 44, 45, 54, 57, 71, 72, 74, 75, 81, 91, 121, 211, 212, 222, 223, 231, 244, 245, 246, 261, 263, 268, 272, 273, 274, 277, 278, 291, 292, 321, 333, 342, 343, 344, 345, 681, 682, 683, 684, 685, 686, 687) # nolint
resource_based_sitc <- c(16, 17, 23, 24, 35, 37, 46, 47, 48, 56, 58, 59, 61, 62, 73, 98, 111, 112, 122, 232, 247, 248, 251, 264, 265, 269, 421, 422, 431, 621, 625, 629, 633, 634, 635, 641, 281, 282, 283, 284, 285, 286, 287, 288, 289, 322, 325, 334, 335, 411, 511, 514, 515, 516, 522, 523, 524, 531, 532, 551, 592, 661, 662, 663, 664, 667, 689) # nolint

# Function to convert SITC codes to HS codes
sitc_to_hs <- function(sitc_codes) {
    hs_codes <- lapply(sitc_codes, function(x) {
        concordance::concord_hs_sitc(sourcevar = x, origin = "SITC4", destination = "HS", dest.digit = 6, all = FALSE)
    }) %>%
        unlist() %>%
        unique()
    return(hs_codes)
}