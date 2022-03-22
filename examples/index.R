library(ggplot2)
library(dplyr)
library(purrr)

index <-
  list(
    data = list(
      "cama" = "https://geodata.md.gov/imap/rest/services/PlanningCadastre/MD_ComputerAssistedMassAppraisal/MapServer/1",
      "cama_bldg" = "https://geodata.md.gov/imap/rest/services/PlanningCadastre/MD_ComputerAssistedMassAppraisal/MapServer/0",
      "parcels" = "https://geodata.md.gov/imap/rest/services/PlanningCadastre/MD_ParcelBoundaries/MapServer/0",
      "edge_of_pavement" = "https://gisdata.baltimorecity.gov/egis/rest/services/OpenBaltimore/Edge_of_Pavement/FeatureServer/0"
      # "property" = "https://opendata.baltimorecity.gov/egis/rest/services/NonSpatialTables/RealProperty/FeatureServer/0"
    ),
    type = "named_intersections",
    package = "mapbaltimore"
  )

params <- list(
  location = list(
    name = "EASTERN AVE & S CONKLING ST",
    label = "Conkling Plaza"
  ),
  data = list(
    dist = 150,
    crs = 3857
  )

)

location <-
  get_location(
    type = index$type,
    package = index$package,
    name = params$location$name,
    label = params$location$label
  )

# FIXME: When this hit an error from the nonspatial data in the FeatureServer link indexed to property
property_data <-
  map_location_data(
    data = names(index$data),
    index = index,
    location = location,
    dist = params$data$dist,
    class = "list",
    crs = params$data$crs
  )

property_data$parcels_cama <-
  property_data$parcels %>%
  sf::st_join(property_data$cama)


mapview_col(property_data,
            nm = "parcels_cama",
            col = "desclu",
            fn = ~ filter()
            )
