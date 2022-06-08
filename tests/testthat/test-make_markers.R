test_that("make_markers works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  markers <- make_markers(nc)

  expect_s3_class(
    markers,
    "sf"
  )

  expect_true(
    is_point(markers)
  )

  marker_df <-
    data.frame(
      name = c(
        "Kenan Memorial Stadium",
        "Downtown Durham YMCA"
      ),
      address = c(
        "104 Stadium Dr, Chapel Hill, NC 27514",
        "218 W Morgan St, Durham, NC 27701"
      ),
      category = c("Sports", "Recreation")
    )

  markers_geocoded <-
    make_markers(
      marker_df,
      geocode = TRUE
    )

  expect_s3_class(
    markers_geocoded,
    "sf"
  )

  group_meta_df <-
    data.frame(
      category = c("Sports", "Recreation"),
      definition = c("About sports.", "About recreation.")
    )

  markers_groupmeta <-
    make_markers(
      markers_geocoded,
      groupname_col = "category",
      group_meta = group_meta_df
    )

  expect_s3_class(
    markers_groupmeta,
    "sf"
  )
})
