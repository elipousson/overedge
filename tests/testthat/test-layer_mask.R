test_that("layer_mask works", {
  library(ggplot2)
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(
    ggplot() +
      geom_sf(data = nc) +
      layer_mask(data = nc[1, ], mask = nc),
    "gg"
  )

  expect_s3_class(
    ggplot() +
      geom_sf(data = nc) +
      layer_mask(data = nc[10, ], diag_ratio = 3, neatline = TRUE),
    "gg"
  )

  expect_s3_class(
    ggplot() +
      geom_sf(data = nc) +
      layer_mask(data = nc[10, ], dist = 100000, neatline = TRUE),
    "gg"
  )
})
