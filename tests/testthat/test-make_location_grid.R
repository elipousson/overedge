test_that("make_location_grid works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  expect_s3_class(
    make_location_grid(nc),
    "sf"
  )

  expect_s3_class(
    make_location_grid(nc, cols = 4, rows = 1),
    "sf"
  )

  expect_s3_class(
    make_location_grid(nc, n = 10),
    "sf"
  )

  expect_s3_class(
    make_location_grid(nc, n = 10, style = "square"),
    "sf"
  )
})
