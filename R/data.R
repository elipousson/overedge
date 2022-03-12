#' @title Map icons
#' @description An index of map icons from four sources:
#' - [mapbox/maki](https://github.com/mapbox/maki)
#' - [ideditor/temaki](https://github.com/ideditor/temaki)
#' - [manifestinteractive/weather-underground-icons](https://github.com/manifestinteractive/weather-underground-icons/)
#' - [openstreetmap/map-icons](https://github.com/openstreetmap/map-icons/)
#' - [openstreetmap/lane-icons](https://github.com/openstreetmap/lane-icons/)
#' - [Esri/calcite-point-symbols](https://github.com/Esri/calcite-point-symbols)
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
#' (MLA)](https://github.com/Esri/calcite-point-symbols#licensing).
#' @format A data frame with 1855 rows and 5 variables:
#' \describe{
#'   \item{\code{name}}{Icon name}
#'   \item{\code{url}}{Icon URL on GitHub repo}
#'   \item{\code{style}}{Icon style (Weather Underground icons only)}
#'   \item{\code{size}}{Icon width/height (pixels)}
#'   \item{\code{repo}}{GitHub repository for icon collection}
#' }
#' @md
"map_icons"


#'  Standard paper and image sizes
#'
#' Reference table of standard paper, postcard, photo print, and social media image
#' sizes, for `get_paper()` and `ggsave_ext()` functions. Derived from
#' [visioguy/PaperSizes](https://github.com/visioguy/PaperSizes/) repo, [Adobe UK
#' guide to photo
#' sizes](https://www.adobe.com/uk/creativecloud/photography/discover/standard-photo-sizes.html)
#' and other sources.
#' @format A data frame with 85 rows and 9 variables:
#' \describe{
#'   \item{\code{name}}{Name of paper}
#'   \item{\code{series}}{Series}
#'   \item{\code{standard}}{Standard}
#'   \item{\code{size}}{Size in series}
#'   \item{\code{units}}{Units ("in", "mm", or "px") for dimensions}
#'   \item{\code{width}}{Width in units}
#'   \item{\code{height}}{Height in units}
#'   \item{\code{asp_portrait}}{Aspect ratio (portrait)}
#'   \item{\code{asp_landscape}}{Aspect ratio (landscape)}
#'   \item{\code{asp_text}}{Aspect ratio ("width:height")}
#'   \item{\code{type}}{Type (paper, postcard, print, or social)}
#' }
"paper_sizes"

#' Valid distance units
#'
#' A subset of units supported by the units package accessible through the
#' units::valid_udunits() function.
#'
#' @format A data frame with 11 rows and 11 variables:
#' \describe{
#'   \item{\code{symbol}}{symbols}
#'   \item{\code{symbol_aliases}}{symbol aliases}
#'   \item{\code{name_singular}}{singular names}
#'   \item{\code{name_singular_aliases}}{singular name aliases}
#'   \item{\code{name_plural}}{character plural names}
#'   \item{\code{name_plural_aliases}}{plural name aliases}
#'   \item{\code{def}}{short definition}
#'   \item{\code{definition}}{definition}
#'   \item{\code{comment}}{comment}
#'   \item{\code{dimensionless}}{logical indicator for dimensionless units}
#'   \item{\code{source_xml}}{source XML}
#' }
"valid_dist_units"


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
#'   \item{\code{scale}}{Scale name}
#'   \item{\code{standard}}{Standard (USGS, architectural, or engineering)}
#'   \item{\code{series}}{Series name (USGS map scales only)}
#'   \item{\code{actual_ft}}{Scale distance for 1 ft actual.}
#'   \item{\code{actual_ft_unit}}{Unit of scale for 1 ft actual.}
#'   \item{\code{scale_in}}{Actual distance for 1 in scale.}
#'   \item{\code{scale_in_unit}}{Unit of actual distance for 1 in scale.}
#'   \item{\code{scale_in_accuracy}}{Accuracy of 1 in scale (approximate or exact)}
#'   \item{\code{scale_cm}}{Actual distance for 1 cm scale.}
#'   \item{\code{scale_cm_unit}}{Unit of actual distance for 1 cm scale.}
#'   \item{\code{scale_cm_accuracy}}{Accuracy of 1 cm scale (approximate or exact)}
#'   \item{\code{size_latlon}}{Standard size in latitude/longitude}
#'   \item{\code{size_latlon_unit}}{Unit of latitude/longitude size (minutes or degrees)}
#'   \item{\code{area_approx}}{Approximate actual area}
#'   \item{\code{area_approx_unit}}{Approximate area unit}
#'   \item{\code{series_status}}{Series status (select USGS map series are "abandoned")}
#' }
"standard_scales"

#' OpenStreetMap building tags
#'
#' Used by `get_osm_data()` if key = "building".
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
#' @format A data frame with 52 rows and 6 variables:
#' \describe{
#'   \item{\code{name}}{State name}
#'   \item{\code{geoid}}{State GeoID}
#'   \item{\code{statefp}}{State FIPS}
#'   \item{\code{abb}}{State abbreviation (USPS)}
#'   \item{\code{bbox}}{Bounding box}
#'   \item{\code{wkt}}{Well known text}
#'}
"us_states"


#' U.S. County boundaries (1:5 mi scale, bbox and wkt)
#'
#' U.S. County boundaries data downloaded with [tigris::counties].
#'
#' @format A data frame with 3220 rows and 7 variables:
#' \describe{
#'   \item{\code{name}}{County name}
#'   \item{\code{geoid}}{County GeoID}
#'   \item{\code{countyfp}}{County FIPS}
#'   \item{\code{statefp}}{State FIPS}
#'   \item{\code{abb_state}}{State abbreviation (USPS)}
#'   \item{\code{bbox}}{Bounding box}
#'   \item{\code{wkt}}{Well known text}
#'}
"us_counties"
