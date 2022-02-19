test_that("sf_to_df works", {
  nc <- st_read(system.file("shape/nc.shp", package = "sf"))

  checkmate::expect_class(
    sf_to_df(x = nc, coords = c("lon", "lat")),
    "data.frame"
  )

  expect_error(
    sf_to_df(x = sf::st_bbox(nc), coords = c("lon", "lat")),
    "no applicable method"
  )

})
