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
#'}
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
#'}
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
#'}
"valid_dist_units"
