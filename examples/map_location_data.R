map_location_data(
  data = c("streets", "mta_bus_lines"),
  package = "mapbaltimore",
  location = mapbaltimore::council_districts[2,],
  label = mapbaltimore::council_districts[2,]$name,
  load = TRUE
)
