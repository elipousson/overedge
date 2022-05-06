# Example
make_location_layers(
  data = get_location_data(
    data = "zoning",
    package = "mapbaltimore"
  ),
  location = get_location(
    type = "council_districts",
    id = 1,
    package = "mapbaltimore"),
  geom = "sf",
  mapping = aes(fill = category_zoning),
  params = list(color = NA, alpha = 0.4)
)
