# FIXME: This is a non-functioning draft example for the layer_location_data function
layers <-
  map_location_data(
    data = list(
      "streets",
      "neighborhoods",
      "council_districts"
    ),
    location = get_location(
      type = "council_districts",
      id = 1,
      package = "mapbaltimore"),
    diag_ratio = 0.2,
    package = "mapbaltimore",
    from_crs = 2804
  )
