# Make a 4 by 5 grid covering a mile buffer around a location
make_location_grid(
  location = mapbaltimore::council_districts[1, ],
  dist = 1,
  unit = "mile",
  cols = 4,
  rows = 5
)

# Make a 2 by 2 grid across a location with a 1000 meter gutter between each cell
make_location_grid(
  location = mapbaltimore::baltimore_city,
  dist = 500,
  unit = "meter",
  cols = 2,
  rows = 2,
  gutter = 1000
)
