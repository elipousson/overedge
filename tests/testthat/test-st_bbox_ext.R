test_that("st_bbox_ext accepts valid parameters and rejects invalid parameters", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  checkmate::expect_class(st_bbox_ext(nc, dist = 100), "bbox")
  checkmate::expect_class(st_bbox_ext(nc, diag_ratio = 0.5), "bbox")
  checkmate::expect_class(st_bbox_ext(nc, asp = "1:2"), "bbox")
  checkmate::expect_class(st_bbox_ext(nc, dist = 100, unit = "feet"), "bbox")
  expect_error(st_bbox_ext(nc, dist = "1"))
  expect_error(st_bbox_ext(nc, asp = "X"))
})
