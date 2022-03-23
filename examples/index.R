library(ggplot2)
library(dplyr)
library(purrr)

property_data$parcels_cama <-
  property_data$parcels %>%
  sf::st_join(property_data$cama)


mapview_col(property_data,
            nm = "parcels_cama",
            col = "desclu",
            fn = ~ filter()
            )
