% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_location_map.R
\name{make_location_map}
\alias{make_location_map}
\alias{make_social_map}
\alias{make_image_map}
\title{Make a ggplot map using layer_location_data}
\usage{
make_location_map(
  location,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  data = NULL,
  crs = NULL,
  paper = "Letter",
  orientation = NULL,
  geom = "sf",
  basemap = TRUE,
  bg_layer = NULL,
  fg_layer = NULL,
  save = FALSE,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  filename = NULL,
  device = NULL,
  path = NULL,
  dpi = 300,
  ...
)

make_social_map(
  location,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  crs = 3857,
  image = NULL,
  platform = NULL,
  format = NULL,
  orientation = NULL,
  basemap = TRUE,
  geom = "mapbox",
  save = FALSE,
  name = NULL,
  filename = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  path = NULL,
  filetype = "jpeg",
  dpi = 72,
  ...
)

make_image_map(
  image_path,
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  data = NULL,
  crs = 3857,
  paper = "Letter",
  orientation = NULL,
  geom = "mapbox",
  style_url = NULL,
  basemap = TRUE,
  bg_layer = NULL,
  fg_layer = NULL,
  save = FALSE,
  name = NULL,
  label = NULL,
  prefix = NULL,
  postfix = NULL,
  filename = NULL,
  device = NULL,
  path = NULL,
  dpi = 300,
  image_geom = "label",
  groupname_col = NULL,
  group_meta = NULL,
  number = FALSE,
  num_by_group = FALSE,
  num_style = NULL,
  num_start = 1,
  suffix = NULL,
  sort = "dist_xmin_ymax",
  desc = FALSE,
  ...
)
}
\arguments{
\item{location}{sf object. If multiple areas are provided, they are unioned
into a single sf object using \link[sf:geos_combine]{sf::st_union}}

\item{dist}{buffer distance in units. Optional.}

\item{diag_ratio}{ratio of diagonal distance of area's bounding box used as
buffer distance. e.g. if the diagonal distance is 3000 meters and the
"diag_ratio = 0.1"  a 300 meter will be used. Ignored when \code{dist} is
provided.}

\item{unit}{Units for buffer. Supported options include "meter", "foot",
"kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
instead of "kilometer") are also supported. Distance in units is converted
to units matching GDAL units for x; defaults to "meter"}

\item{asp}{Aspect ratio of width to height as a numeric value (e.g. 0.33) or
character (e.g. "1:3"). If numeric, \code{\link[=get_asp]{get_asp()}} returns the same value
without modification.}

\item{data}{Character string (e.g. url, file path, or name of data from
package), a \code{sf}, \code{sfc}, or \code{bbox}  object including data in area.}

\item{crs}{Coordinate reference system of bounding box to return}

\item{paper}{Paper, Default: 'letter'.}

\item{orientation}{Orientation "portrait", "landscape", or "square", Default:
'portrait'.}

\item{geom}{A character string indicating which ggplot2 geom to use, Default:
'sf'. Options include "sf" (\link[ggplot2:ggsf]{ggplot2::geom_sf}), "icon" (\link{layer_icon}),
"markers" (\link{layer_markers}), "text" (\link[ggplot2:ggsf]{ggplot2::geom_sf_text}), and "label"
(\link[ggplot2:ggsf]{ggplot2::geom_sf_label}). See details for a full list.}

\item{basemap}{If \code{FALSE}, return a list of ggplot2 layers (or ggproto
objects). If \code{TRUE}, add the list to \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} to return a map.}

\item{bg_layer, fg_layer}{A ggplot2 layer or a list of ggproto objects (e.g.
scales, labels, etc.) to add to the background or foreground of the primary
map layer defined by \code{"geom"} and other parameters.}

\item{save}{If \code{TRUE}, save file with \link{ggsave_ext} or \link{ggsave_social},
requires \code{basemap = TRUE} and filename is not NULL \emph{or} ... include a name
parameter. Default: \code{FALSE}}

\item{name}{Plot name, used to create filename (if filename is \code{NULL}) using
\code{\link[=make_filename]{make_filename()}}}

\item{label}{Label to combine with name converted to snake case with
\code{\link[janitor:make_clean_names]{janitor::make_clean_names()}}. The label is designed to identify the area
or other shared characteristics across multiple data files, maps, or plots.}

\item{prefix}{File name prefix. "date" adds a date prefix, "time" adds a
date/time prefix; defaults to \code{NULL}.}

\item{postfix}{File name postfix; defaults to \code{NULL}.}

\item{filename}{File name to create on disk.}

\item{device}{Device to use. Can either be a device function
(e.g. \link{png}), or one of "eps", "ps", "tex" (pictex),
"pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).}

\item{path}{Path of the directory to save plot to: \code{path} and \code{filename}
are combined to create the fully qualified file name. Defaults to the
working directory.}

\item{dpi}{Plot resolution. Also accepts a string input: "retina" (320),
"print" (300), or "screen" (72). Applies only to raster output types.}

\item{...}{Additional parameters passed to \link{layer_location_data}.}

\item{image}{Image size name, Default: \code{NULL}}

\item{platform}{Social media platform, "Instagram", "Facebook", or "Twitter",
Default: \code{NULL}}

\item{format}{Image format, "post", "story", or "cover", Default: \code{NULL}}

\item{filetype}{File type to use if passing parameters to \link{read_sf_download}
or \link{read_sf_pkg} (required for extdata and cached data).}

\item{image_path}{path to location of images for \code{\link[=make_image_map]{make_image_map()}}}

\item{style_url}{A Mapbox style url; defaults to \code{NULL}.}

\item{image_geom}{For \code{\link[=make_image_map]{make_image_map()}}, geom to use with layer_markers to
mark the location of images (based on EXIF metadata).}

\item{groupname_col}{Group column name, used to join group metadata if
group_meta is a non-spatial data frame; Default: \code{NULL}}

\item{group_meta}{Group metadata as a data frame or sf object that intersect
with markers; Default: \code{NULL}}

\item{number}{If \code{TRUE}, number markers using \code{\link[=layer_markers]{layer_markers()}} (not
currently supported)}

\item{num_by_group}{If \code{TRUE}, numbers are added by group based on
groupname_col.}

\item{num_style}{Style of enumeration, either "arabic", "alph", "Alph",
"roman", "Roman"}

\item{num_start}{Starting number; defaults to 1.}

\item{suffix}{Character to appended to "number" column. (e.g. "." for "1." or
":" for "1:"). Can also be a character vector with the same length as the
number column.}

\item{sort}{Sort column name, Default: "dist_xmin_ymax".}

\item{desc}{If \code{TRUE}, sort descending; default \code{FALSE}.}
}
\description{
Location is used as the data parameter of layer_location_data so this
function is primarily appropriate for the layer_mapbox (\code{geom = "mapbox"}).
}
\details{
Using make_image_map:

\link{make_image_map} wraps \link{read_sf_exif} and \link{make_location_map}. It is designed for
making simple maps of photos in combination with reference tables.
}
