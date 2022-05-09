nc <- read_sf_path(system.file("shape/nc.shp", package = "sf"))

raleigh_msa <-
  get_location(
    type = nc,
    name_col = "NAME",
    name =  c("Franklin", "Johnston", "Wake"),
    crs = 3857
    )

ggplot2::ggplot() +
  layer_frame(
    data = raleigh_msa,
    frame = "circle",
    fill = "lightyellow",
    inscribed = FALSE
  ) +
  layer_location_data(
    data = raleigh_msa,
    mapping = ggplot2::aes(fill = NAME),
    alpha = 0.5
  ) +
  ggplot2::guides(
    fill = "none"
  )
