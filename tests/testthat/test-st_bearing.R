test_that("st_bearing works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_pts <- suppressWarnings(sf::st_centroid(nc))

  line <-
    sf::st_as_sf(
      sf::st_cast(sf::st_union(c(nc_pts[1, ]$geometry, nc_pts[30, ]$geometry)), "LINESTRING")
    )
})
