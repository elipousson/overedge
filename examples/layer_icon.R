nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

basemap <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  layer_location_data(data = nc)

basemap +
  geom_sf_icon(data = nc, icon = "point-start", size = 10)

nc$icon <- rep(c("1", "2", "3", "4"), nrow(nc) / 4)

basemap +
  geom_sf_icon(data = nc, size = 5)
