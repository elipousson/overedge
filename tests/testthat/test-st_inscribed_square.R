test_that("st_inscribed_square works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc_6543 <- sf::st_transform(nc, 6543)
  checkmate::expect_class(st_inscribed_square(nc_6543), "sf")
  expect_error(st_inscribed_square(nc), "st_inscribed_square does not work")
  expect_error(st_inscribed_square("x"))
})
