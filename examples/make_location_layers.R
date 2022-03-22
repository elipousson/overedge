# Example
make_location_layers(
  data = mapbaltimore::zoning,
  location = mapbaltimore::baltimore_city,
  geom = "sf",
  mapping = aes(fill = category_zoning),
  params = list(color = NA, alpha = 0.4)
)
