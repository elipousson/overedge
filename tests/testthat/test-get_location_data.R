test_that("get_location_data works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  checkmate::expect_class(
    get_location_data(
      location = nc[1, ],
      dist = 50,
      units = "mi",
      asp = 1,
      data = nc
    ),
    "sf"
  )

  checkmate::expect_class(
    get_location_data(
      location = get_location(
        type = "neighborhoods",
        package = "mapbaltimore",
        location = "100 Holliday St, Baltimore, MD 21202"
      ),
      dist = 0.25,
      units = "mi",
      asp = 1,
      data = "neighborhoods",
      package = "mapbaltimore"
    ),
    "sf"
  )
})
