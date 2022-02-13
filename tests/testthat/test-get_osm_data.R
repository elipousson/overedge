test_that("get_osm_data works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_parks <- get_osm_data(location = nc[1,], key = "leisure", value = "park", osmdata = TRUE)
  checkmate::test_class(nc_parks, "osmdata") # Check class with default param
  checkmate::test_class(nc_parks$osm_polygons, "sf") # Check class with default params
  nc_barns <- get_osm_data(location = nc[87,], key = "building", value = "barn", crs = 4326)
  checkmate::test_class(nc_barns, "sf")
})
