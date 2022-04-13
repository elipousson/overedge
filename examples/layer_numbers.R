ggplot2::ggplot() +
  layer_location_data(
    data = mapbaltimore::baltimore_city_detailed,
    fill = NA
  ) +
  layer_numbers(
    data = mapbaltimore::csas,
    sort = "dist_xmid_ymid",
    num_style = "arabic",
    geom = "text",
    size = 5) +
  layer_neatline(
    data = mapbaltimore::baltimore_city,
    asp = 1,
    dist = 1,
    unit = "mi",
    expand = TRUE
  )
