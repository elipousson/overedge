# FIXME: This is a non-functioning draft example for the layer_location_data function
layers <-
  layer_location_data(
    data = list(
      "streets",
      "neighborhoods",
      "council_districts"
    ),
    mapping = NULL,
    location = mapbaltimore::council_districts[1, ],
    diag_ratio = 0.2,
    package = "mapbaltimore",
    fill = NA,
    geom = "sf"
  )
