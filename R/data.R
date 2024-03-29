#' @title Map icons
#' @description An index of map icons from four sources:
#' - [mapbox/maki](https://github.com/mapbox/maki)
#' - [ideditor/temaki](https://github.com/ideditor/temaki)
#' - [manifestinteractive/weather-underground-icons](https://github.com/manifestinteractive/weather-underground-icons/)
#' - [openstreetmap/map-icons](https://github.com/openstreetmap/map-icons/)
#' - [openstreetmap/lane-icons](https://github.com/openstreetmap/lane-icons/)
#' - [Esri/calcite-point-symbols](https://github.com/Esri/calcite-point-symbols)
#' - [NPMap Symbol Library](https://github.com/nationalparkservice/symbol-library/)
#'
#' Most of these icon sources use open licenses.
#' [Maki](https://github.com/mapbox/maki/blob/main/LICENSE.txt),
#' [Temaki](https://github.com/ideditor/temaki/blob/main/LICENSE.md), and the
#' [OSM lane icons](https://github.com/openstreetmap/lane-icons/blob/master/LICENSE.md)
#' all use a CC0 license. The Weather Underground Icons use an
#' [MIT
#' license](https://github.com/manifestinteractive/weather-underground-icons/blob/master/LICENSE).
#' The OSM map icons use an unspecified PD style license.
#' The Calcite icons are [available under the Esri Master License Agreement
#' (MLA)](https://github.com/Esri/calcite-point-symbols#licensing). The NPMap
#' Symbol Library is created by the National Park Service so is assumed to be a
#' [Public Domain work in the U.S.](http://www.usa.gov/publicdomain/label/1.0/).
#' @format A data frame with 1855 rows and 5 variables:
#' \describe{
#'   \item{`name`}{Icon name}
#'   \item{`url`}{Icon URL on GitHub repo}
#'   \item{`style`}{Icon style (Weather Underground icons only)}
#'   \item{`size`}{Icon width/height (pixels)}
#'   \item{`repo`}{GitHub repository for icon collection}
#' }
"map_icons"


#'  Standard paper and image sizes
#'
#' Reference table of standard paper, postcard, photo print, and social media image
#' sizes, for [get_paper] and [ggsave_ext] functions. Derived from
#' [visioguy/PaperSizes](https://github.com/visioguy/PaperSizes/) repo, [Adobe UK
#' guide to photo
#' sizes](https://www.adobe.com/uk/creativecloud/photography/discover/standard-photo-sizes.html)
#' and other sources.
#' @format A data frame with 85 rows and 9 variables:
#' \describe{
#'   \item{`name`}{Name of paper}
#'   \item{`series`}{Series}
#'   \item{`standard`}{Standard}
#'   \item{`size`}{Size in series}
#'   \item{`units`}{Units ("in", "mm", or "px") for dimensions}
#'   \item{`width`}{Width in units}
#'   \item{`height`}{Height in units}
#'   \item{`orientation`}{Portrait (width greater than height), landscape, or square}
#'   \item{`type`}{Type (paper, postcard, print, or social)}
#' }
"paper_sizes"

#' Distance units (data frame)
#'
#' A subset of units supported by the units package accessible through the
#' [units::valid_udunits] function.
#'
#' @format A data frame with 11 rows and 11 variables:
#' \describe{
#'   \item{`symbol`}{symbols}
#'   \item{`symbol_aliases`}{symbol aliases}
#'   \item{`name_singular`}{singular names}
#'   \item{`name_singular_aliases`}{singular name aliases}
#'   \item{`name_plural`}{character plural names}
#'   \item{`name_plural_aliases`}{plural name aliases}
#'   \item{`def`}{short definition}
#'   \item{`definition`}{definition}
#'   \item{`comment`}{comment}
#'   \item{`dimensionless`}{logical indicator for dimensionless units}
#'   \item{`source_xml`}{source XML}
#' }
"dist_units"

#' Standard map, architectural, and engineering scales
#'
#' Standard map scales derived from USGS 2002 report on map scales
#' <https://pubs.usgs.gov/fs/2002/0015/report.pdf>
#'
#' Common architectural and engineering scales derived from FEMA guide to using
#' scales
#' <https://www.usfa.fema.gov/downloads/pdf/nfa/engineer-architect-scales.pdf>
#'
#' @format A data frame with 32 rows and 16 variables:
#' \describe{
#'   \item{`scale`}{Scale name}
#'   \item{`standard`}{Standard (USGS, architectural, or engineering)}
#'   \item{`series`}{Series name (USGS map scales only)}
#'   \item{`actual_ft`}{Scale distance for 1 ft actual.}
#'   \item{`actual_ft_unit`}{Unit of scale for 1 ft actual.}
#'   \item{`scale_in`}{Actual distance for 1 in scale.}
#'   \item{`scale_in_unit`}{Unit of actual distance for 1 in scale.}
#'   \item{`scale_in_accuracy`}{Accuracy of 1 in scale (approximate or exact)}
#'   \item{`scale_cm`}{Actual distance for 1 cm scale.}
#'   \item{`scale_cm_unit`}{Unit of actual distance for 1 cm scale.}
#'   \item{`scale_cm_accuracy`}{Accuracy of 1 cm scale (approximate or exact)}
#'   \item{`size_latlon`}{Standard size in latitude/longitude}
#'   \item{`size_latlon_unit`}{Unit of latitude/longitude size (minutes or degrees)}
#'   \item{`area_approx`}{Approximate actual area}
#'   \item{`area_approx_unit`}{Approximate area unit}
#'   \item{`series_status`}{Series status (select USGS map series are "abandoned")}
#' }
"standard_scales"

#' OpenStreetMap building tags
#'
#' Used by [get_osm_data] if key = "building".
#'
#' More information on the building key
#' <https://wiki.openstreetmap.org/wiki/Key:building>
#'
#' @format A character vector with length of 84
"osm_building_tags"

#' U.S. State boundaries (1:5 mi scale, bbox and wkt)
#'
#' U.S. State boundaries data downloaded with [tigris::states].
#'
#' The geoid and wkt columns are labelled with the state abbreviation (abb) in
#' lower case.
#'
#' @format A data frame with 52 rows and 6 variables:
#' \describe{
#'   \item{`name`}{State name}
#'   \item{`geoid`}{State GeoID (labelled)}
#'   \item{`abb`}{State abbreviation (USPS)}
#'   \item{`est_pop`}{Estimated state population (B01001_001), American Community Survey 5 year, 2015-2019}
#'   \item{`statefp`}{State FIPS}
#'   \item{`bbox`}{Bounding box}
#'   \item{`wkt`}{Well known text (labelled)}
#' }
"us_states"


#' U.S. County boundaries (1:5 mi scale, bbox and wkt)
#'
#' U.S. County boundaries data downloaded with [tigris::counties].
#'
#' The geoid and wkt columns are labelled with a combination of an abbreviated
#' county name and state abbreviation in snake case. Population estimates
#' (est_pop) are not included for county equivalents in the U.S. Virgin Islands,
#' Guam, Northern Mariana Islands, and American Samoa.
#'
#' @format A data frame with 3220 rows and 7 variables:
#' \describe{
#'   \item{`name`}{County name (tidycensus)}
#'   \item{`name_short`}{County name without state (tigris)}
#'   \item{`geoid`}{County GeoID (labelled)}
#'   \item{`abb_state`}{State abbreviation (USPS)}
#'   \item{`est_pop`}{Estimated county population (B01001_001), American Community Survey 5 year, 2015-2019}
#'   \item{`countyfp`}{County FIPS}
#'   \item{`statefp`}{State FIPS}
#'   \item{`bbox`}{Bounding box}
#'   \item{`wkt`}{Well known text (labelled)}
#' }
"us_counties"

#' Distance units (vector)
#'
#' A vector of supported distance units pulled from [dist_units].
#'
#' @format A character vector with 55 names, plural names, aliases, and symbols
#'   for distance units.
"dist_unit_options"

#' Area units (vector)
#'
#' A vector of supported area units derived from [dist_units] and
#' [units::valid_udunits].
#'
#' @format A character vector with 35 names, plural names, and aliases for area
#'   units.
"area_unit_options"
