test_that("get_paper works", {
  expect_identical(
    get_paper("letter")$name,
    "Letter"
  )

  expect_identical(
    get_paper(paper = NULL, standard = "ISO", series = "A", size = 4)$width,
    210
  )

  expect_identical(
    nrow(get_paper(width = 11, height = 17)),
    3L
  )
})
