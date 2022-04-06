test_that("layer_neatline works", {
  library(ggplot2)
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_map <- ggplot(nc) +
    geom_sf()
  expect_s3_class(nc_map + layer_neatline(data = nc[1, ]), "ggplot") # Check class with default params
  expect_error(nc_map + layer_neatline(data = nc[1, ], dist = "one")) # Check bad dist parameter
  expect_s3_class(nc_map + layer_neatline(data = nc[1, ], expand = TRUE), "ggplot") # Check class with expand parameter TRUE
})
