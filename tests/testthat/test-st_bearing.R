test_that("st_bearing works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_pts <- suppressWarnings(sf::st_centroid(nc))

  line <-
    sf::st_as_sf(
      sf::st_cast(sf::st_union(c(nc_pts[1, ]$geometry, nc_pts[30, ]$geometry)), "LINESTRING")
    )


  lines <-
    get_osm_data(
      location = get_location(
        type = "neighborhoods",
        package = "mapbaltimore",
        name = "Harwood"),
      key = "highway",
      value = "tertiary",
      geometry = "lines"
    )

  checkmate::expect_class(
    st_bearing(x = line, crs = 4326),
    "sf"
  )

  checkmate::expect_class(
    st_bearing(x = lines),
    "sf"
  )
})
