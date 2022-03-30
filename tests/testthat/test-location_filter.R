test_that("location_filter works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  testthat::expect_s3_class(location_filter(data = nc, location = "37001"), "sf")
  testthat::expect_equal(nrow(location_filter(data = nc, location = "16 W. Jones Street, Raleigh, NC 27601")), 1)
  testthat::expect_equal(nrow(location_filter(data = nc, location = nc[1, ], trim = TRUE)), 4)
})
