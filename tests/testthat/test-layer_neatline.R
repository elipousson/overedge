test_that("layer_neatline works", {
  library(ggplot2)
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_map <- ggplot(nc) + geom_sf()
  checkmate::expect_class(nc_map + layer_neatline(data = nc[1,]), "ggplot") # Test output
  expect_error(nc_map + layer_neatline(data = nc[1,], dist = "one"))  # Test bad parameter value for st_bbox_adj
})
