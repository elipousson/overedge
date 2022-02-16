test_that("st_bearing works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_lines <-
    suppressWarnings(
      sf::st_cast(sf::st_cast(nc, "MULTILINESTRING"), "LINESTRING")
    )

  checkmate::expect_class(
    st_bearing(x = nc_lines),
    "sf"
  )

  checkmate::expect_class(
    st_bearing(x = nc_lines, dir = TRUE),
    "sf"
  )
})
