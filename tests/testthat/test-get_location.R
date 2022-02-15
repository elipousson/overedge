test_that("get_location works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  # Check if type as sf object with name/id lookup works
  checkmate::test_class(
    get_location(type = nc, name = "Warren", name_col = "NAME"),
    "sf"
  )

  checkmate::test_class(
    get_location(type = nc, id = 37185, id_col = "FIPSNO"), "sf"
  )


  checkmate::test_class(
    get_location(type = nc, id = "37185", id_col = "FIPSNO"), "sf"
  )

  # Check if type as path works
  checkmate::test_class(
    get_location(
      type = system.file("shape/nc.shp", package = "sf"),
      name = "Hertford",
      name_col = "NAME"
    ),
    "sf"
  )


  checkmate::test_class(
    get_location(
      type = nc,
      name = c("Ashe", "Alleghany", "Surry"),
      name_col = "NAME",
      union = TRUE,
      label = "Three NC counties"
    ),
    "sf"
  )


  # Check if index list works

  checkmate::test_class(
    get_location(
      type = "smaller",
      name = "Hertford",
      name_col = "NAME",
      index = list(
        "smaller" = dplyr::filter(nc, AREA <= 0.10),
        "larger" = dplyr::filter(nc, AREA > 0.15)
      )
    ),
    "sf"
  )

  # Check passing an sf object to location
  checkmate::test_class(
    get_location(
      type = sf::st_transform(nc, 6542),
      location = st_buffer_ext(sf::st_transform(nc[1,], 6542), 250)
    ),
    "sf"
  )

  # Check if type as url works with passing extra parameters to get_location_data()
  # In this case, no location information is passed to get_location() so it warns before returning all types

  checkmate::test_class(
    get_location(
      type = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/baltimore-city-wards-1797.geojson",
      name = "1st Ward"
    ),
    "sf"
  )


  checkmate::test_class(
    get_location(
      type = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/baltimore-city-wards-1802.geojson",
      location = "100 Holliday St, Baltimore, MD 21202"
    ),
    "sf"
  )

  checkmate::test_class(
    get_location(
      type = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/baltimore-city-wards-1802.geojson",
      location = "100 Holliday St, Baltimore, MD 21202"
    ),
    "sf"
  )


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

  checkmate::test_class(get_with_type_and_package, "sf")

  expect_warning(
    get_location(
      type = "neighborhoods",
      name = "Harwood",
      package = "mapbaltimore",
      index = "This index is not a list."
    ),
    "index must be a list or NULL"
  )
})
