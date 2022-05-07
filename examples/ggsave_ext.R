\dontrun{
  ggplot2::ggplot() +
    layer_location_data(
      data = system.file("shape/nc.shp", package = "sf")
    )

  ggsave_ext(
    name = "counties",
    label = "North Carolina",
    device = "pdf",
    paper = "letter"
    )
}
