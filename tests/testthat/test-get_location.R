test_that("get_location works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  # Check if type as sf object with name/id lookup works
  get_by_name <- get_location(type = nc, name = "Warren", name_col = "NAME")

  checkmate::expect_class(get_by_name, "sf")

  get_by_id <- get_location(type = nc, id = 37185, id_col = "FIPSNO")

  checkmate::expect_class(get_by_id, "sf")

  # Check if type as path works
  get_with_type_path <- get_location(
    type = system.file("shape/nc.shp", package = "sf"),
    name = "Hertford",
    name_col = "NAME"
  )

  checkmate::expect_class(get_with_type_path, "sf")

  # Check if index list works
  get_with_type_index <- get_location(
    type = "smaller",
    name = "Hertford",
    name_col = "NAME",
    index = list(
      "smaller" = dplyr::filter(nc, AREA <= 0.10),
      "larger" = dplyr::filter(nc, AREA > 0.15)
    )
  )

  checkmate::expect_class(get_with_type_index, "sf")

  # Check if type as url works with passing extra parameters to get_location_data()
  # In this case, no location information is passed to get_location() so it warns before returning all types
  get_with_type_url <-
    get_location(
      type = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/baltimore-city-wards-1797.geojson",
      name = "1st Ward"
    )

  checkmate::expect_class(get_with_type_url, "sf")

  expect_warning(
    get_location(
      type = "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Parks/FeatureServer/0",
      locationname_col = "NAME",
      locationname = "Chesapeake and Ohio Canal National Historic Park"
    ),
    "No location is provided"
  )

  # Get mapbaltimore
  get_with_type_and_package <- get_location(
    type = "neighborhoods",
    name = "Harwood",
    package = "mapbaltimore"
  )

  checkmate::expect_class(get_with_type_and_package, "sf")
})
