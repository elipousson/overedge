nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
# Check if type as sf object with name/id lookup works
get_location(type = nc, name = "Warren", name_col = "NAME")
get_location(type = nc, id = 37185, id_col = "FIPSNO")

# Type also supports a range of other formats including

# File path
get_location(
  type = system.file("shape/nc.shp", package = "sf"),
  name = "Hertford",
  name_col = "NAME"
)

# Index name (if a named list of datasets, urls, or paths is passed to index)
get_location(
  type = "smaller",
  name = "Hertford",
  name_col = "NAME",
  index = list(
    "smaller" = dplyr::filter(nc, AREA <= 0.10),
    "larger" = dplyr::filter(nc, AREA > 0.15)
  )
)


