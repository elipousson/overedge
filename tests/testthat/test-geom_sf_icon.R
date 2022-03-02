test_that("geom_sf_icon works", {
  library(ggplot2)
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_icon <- nc
  nc_icon$icon <- rep(c("1", "2", "3", "4"), nrow(nc) / 4)

  checkmate::expect_class(
    ggplot() +
      geom_sf_icon(data = nc_icon, size = 5),
    "gg"
  )

  nc_pts <- suppressWarnings(sf::st_centroid(nc))

  checkmate::expect_class(
    ggplot() +
      geom_sf_icon(data = nc_pts, icon = "point-start", size = 10),
    "gg"
  )

  checkmate::expect_class(
    ggplot() +
      geom_sf_icon(data = nc_pts, icon = "building", source = "mapbox/maki"),
    "gg"
  )

  checkmate::expect_class(
    ggplot() +
      geom_sf_icon(data = nc_pts, svg = "https://raw.githubusercontent.com/ideditor/temaki/main/icons/island_trees_building.svg"),
    "gg"
  )

  expect_error(
    ggplot() +
      geom_sf_icon(data = nc_pts, icon = "building"),
    "The provided parameters match more than one icon"
  )
})
