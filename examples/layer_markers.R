nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))
nc <- st_transform_ext(nc, 3857)

basemap <-
  ggplot2::ggplot() +
  layer_location_data(
    data = nc,
    fill = NA
  ) +
  ggplot2::theme_void()

basemap +
  layer_markers(
    data = nc[1:10],
    mapping = ggplot2::aes(size = AREA),
    make = TRUE
  )

large_nc <-
  get_location_data(
    data = nc,
    fn = ~ dplyr::filter(.x, AREA > 0.2)
  )

basemap +
  layer_numbers(
    data = large_nc,
    mapping = ggplot2::aes(fill = NAME),
    sort = "dist_xmid_ymid",
    num_style = "Roman",
    geom = "label",
    size = 3
    ) +
  ggplot2::guides(fill = "none")
