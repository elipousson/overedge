test_that("get_asp works", {
  expect_equal(get_asp("1:2"), 0.5)
  expect_equal(get_asp(11 / 17), 0.6470588, tolerance = 0.00001)
  expect_equal(get_asp(paper = "letter"), 0.7727273, tolerance = 0.00001)
  expect_equal(get_asp(paper = "letter", margin = "none", unit = "in", block_asp = TRUE), 0.7727273, tolerance = 0.00001)
  expect_equal(get_asp(paper = "letter", margin = c(1.75, .5, 1.75, .5), unit = "in", block_asp = TRUE), 1)
})
