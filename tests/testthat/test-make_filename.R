test_that("make_filename works", {
  expect_identical(
    class(make_filename(
      name = "ABC",
      prefix = "datetime",
      postfix = "XYZ"
    )), "character"
  )

  expect_error(
    make_filename(
      name = NULL,
      prefix = "datetime",
      postfix = "XYZ"
    ), "is.character(name) || is.character(filename) is not TRUE"
  )
})
