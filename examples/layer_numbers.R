nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc <- st_transform_ext(nc, 3857)

ggplot2::ggplot() +
  layer_location_data(
    data = nc,
    fill = NA
  ) +
  layer_numbers(
    data = dplyr::slice_sample(nc, n = 25),
    sort = "dist_xmid_ymid",
    num_style = "arabic",
    geom = "label",
    size = 3
    ) +
  ggplot2::theme_void()
