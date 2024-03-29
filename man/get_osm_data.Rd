% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_osm_data.R
\name{get_osm_data}
\alias{get_osm_data}
\alias{get_osm_id}
\alias{get_osm_boundaries}
\title{Use osmdata to get Open Street Map data for a location}
\usage{
get_osm_data(
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  key,
  value = NULL,
  features = NULL,
  crs = NULL,
  geometry = NULL,
  osmdata = FALSE,
  enclosing = NULL,
  nodes_only = FALSE,
  timeout = 120
)

get_osm_id(id, type = NULL, crs = NULL, geometry = NULL, osmdata = FALSE)

get_osm_boundaries(
  location,
  level = NULL,
  lang = "en",
  crs = NULL,
  enclosing = "relation",
  geometry = NULL,
  osmdata = FALSE
)
}
\arguments{
\item{location}{A \code{sf}, \code{sfc}, or \code{bbox} object.}

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

\item{key}{Feature key for overpass API query.}

\item{value}{Value of the feature key; can be negated with an initial
exclamation mark, \code{value = "!this"}, and can also be a vector, \code{value = c("this", "that")}. If \code{value = "all"} or if \code{key = "building"} the values
passed to the osmdata package are from a preset list extracted from
\code{\link[osmdata:available_tags]{osmdata::available_tags()}}.}

\item{features}{Character vector of key-value pairs with keys and values
enclosed in escape-formatted quotations (see examples).}

\item{crs}{Coordinate reference system for output data; if \code{NULL}, the data
remains in the Open Street Map coordinate reference system 4326. Default:
\code{NULL}.}

\item{geometry}{Geometry type to output ("polygons", "points", "lines",
"multilines", or "multipolygons"); if multiple geometry types are needed
set osmdata to \code{TRUE.} Default \code{NULL}.}

\item{osmdata}{If \code{TRUE} return a \code{osmdata} class object that includes the
overpass API call, metadata including timestamp and version numbers, and
all available geometry types; defaults to \code{FALSE}.}

\item{enclosing}{If enclosing is "relation" or "way", use the
\code{\link[osmdata:opq_enclosing]{osmdata::opq_enclosing()}} function to query the OSM data (instead of
\code{\link[osmdata:add_osm_feature]{osmdata::add_osm_feature()}}. Defaults to \code{NULL}. When using enclosing, the
dist, diag_ratio, asp, and unit parameters are ignored and the center of
the provided location is used for the query. geometry is set automatically
based enclosing with "relation" using "multipolygon" and "way" using
"polygon" geometry.}

\item{nodes_only}{If \code{TRUE}, query OSM nodes only. Some OSM structures such
as \code{place = "city"} or \code{highway = "traffic_signals"} are represented by
nodes only. Queries are built by default to return all nodes, ways, and
relation, but this can be very inefficient for node-only queries.
Setting this value to \code{TRUE} for such cases makes queries more
efficient, with data returned in the \code{osm_points} list item.}

\item{timeout}{It may be necessary to increase this value for large queries,
because the server may time out before all data are delivered.}

\item{id}{OpenStreetMap feature id}

\item{type}{type of feature with id; "node", "way", or "relation"}

\item{level}{administrative level (admin_level) of boundary to return;
defaults to \code{NULL}. See \url{https://wiki.openstreetmap.org/wiki/Key:admin_level}
for more information. Only used for \link{get_osm_boundaries}.}

\item{lang}{Language for boundary names to include in resulting data frame
(e.g. "en" for English or "es" for Spanish). Default language names should
always be included in results. Defaults to "en". See
\url{https://wiki.openstreetmap.org/wiki/Multilingual_names} for more
information.}
}
\value{
A simple feature object with features using selected geometry type or
an \code{osmdata} object with features from all geometry types.
}
\description{
Wraps \code{osmdata} functions to query OSM data by adjusted bounding box or
by enclosing ways/relations around the center of a location. timeout and
nodes_only parameters are not fully supported currently.
}
