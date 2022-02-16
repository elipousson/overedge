test_that("df_to_sf works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_df <- ggspatial::df_spatial(nc)
  nc_sf <- df_to_sf(nc_df, coords = c("x", "y"))
  checkmate::expect_class(nc_sf, "sf")
  nc_df$xy <- paste(nc_df$x, nc_df$y, sep = ",")
  nc_sf_xy <- df_to_sf(nc_df, coords = "xy", into = c("lon", "lat"))
  expect_equal(sf::st_bbox(nc_sf), sf::st_bbox(nc_sf_xy))
})
