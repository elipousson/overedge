test_that("sf_bbox functions work", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_bbox <- sf::st_bbox(nc)
  checkmate::expect_class(sf_bbox_to_sf(nc_bbox), "sf")
  checkmate::expect_character(sf_bbox_to_wkt(nc_bbox), pattern = "^POLYGON")
  checkmate::expect_character(sf_bbox_to_lonlat_query(nc_bbox))
  expect_error(sf_bbox_to_lonlat_query("nc_bbox"))
  # Some functions also work with sf objects directly
  # expect_error(sf_bbox_to_sf(nc))
  expect_length(sf_bbox_to_wkt(nc), 100)
})
