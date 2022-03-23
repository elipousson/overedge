test_that("is_gg works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  nc_layer <- ggplot2::geom_sf(data = nc)
  nc_map <- ggplot2::ggplot() + nc_layer
  nc_map_list <- list(nc_map)

  expect_true(is_gg_layer(nc_layer))
  expect_true(is_gg_sf_layer(nc_layer))
  expect_true(is_gg_scale(ggplot2::scale_fill_brewer()))
  expect_true(is_gg_theme(ggplot2::theme_minimal()))
  expect_true(is_gg_component(nc_layer))
  expect_true(is_gg_list(nc_map_list))
})
