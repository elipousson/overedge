nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

ggplot2::ggplot() +
  layer_location_data(data = nc) +
  layer_neatline(data = nc[1, ], asp = 1, color = "red", size = 1.5, linetype = "dashed")

ggplot2::ggplot() +
  layer_location_data(data = nc) +
  layer_neatline(data = nc[1, ], dist = 20, unit = "mi", color = "none")
