test_that("get_us_counties works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))

  county_list <- get_us_counties(location = nc[1,], dist = 5, unit = "mi", historical = TRUE)

  expect_identical(nrow(county_list), 74L)
  checkmate::expect_class(county_list, "sf")
})
