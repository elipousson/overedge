nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc <- st_transform_ext(nc, 3857)

ggplot2::ggplot() +
  layer_frame(
    data = dplyr::filter(nc, NAME %in% c("Franklin", "Johnston", "Wake")),
    frame = "circle",
    fill = "lightyellow",
    inscribed = FALSE
  ) +
  layer_location_data(
    data = dplyr::filter(nc, NAME %in% c("Franklin", "Johnston", "Wake")),
    mapping = ggplot2::aes(fill = NAME),
    alpha = 0.5
  ) +
  ggplot2::guides(
    fill = "none"
  )
