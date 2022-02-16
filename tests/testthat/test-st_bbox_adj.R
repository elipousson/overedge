test_that("st_bbox_adj accepts valid parameters and rejects invalid parameters", {
  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
  checkmate::expect_class(st_bbox_adj(nc, dist = 100), "bbox")
  checkmate::expect_class(st_bbox_adj(nc, diag_ratio = 0.5), "bbox")
  checkmate::expect_class(st_bbox_adj(nc, asp = "1:2"), "bbox")
  checkmate::expect_class(st_bbox_adj(nc, dist = 100, unit = "feet"), "bbox")
  expect_error(st_bbox_adj(nc, dist = "1"))
  expect_error(st_bbox_adj(nc, asp = "X"))
})
