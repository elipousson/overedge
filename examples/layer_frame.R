ggplot2::ggplot() +
  layer_frame(
    data = mapbaltimore::baltimore_msa_counties,
    frame = "circle",
    fill = "lightblue",
    alpha = 0.5,
    dist = 1.5,
    unit = "mile",
    inscribed = FALSE
  ) +
  layer_location_data(
    data = mapbaltimore::baltimore_msa_counties,
    mapping = ggplot2::aes(fill = name),
    alpha = 0.5
  ) +
  ggplot2::guides(
    fill = "none"
  )
