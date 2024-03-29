% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_filter.R
\name{location_filter}
\alias{location_filter}
\title{Filter, crop, or trim data to a location}
\usage{
location_filter(
  data,
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  asp = NULL,
  unit = "meter",
  crs = NULL,
  trim = FALSE,
  crop = TRUE,
  ...
)
}
\arguments{
\item{data}{Data to filter by location.}

\item{location}{A sf, sfc, or bbox object or a character string that is an
address, county GeoID, state name, abbreviation, or GeoID (dist parameters
are ignored if location is a character string).}

\item{dist}{buffer distance in units. Optional.}

\item{diag_ratio}{ratio of diagonal distance of area's bounding box used as
buffer distance. e.g. if the diagonal distance is 3000 meters and the
"diag_ratio = 0.1"  a 300 meter will be used. Ignored when \code{dist} is
provided.}

\item{asp}{Aspect ratio of width to height as a numeric value (e.g. 0.33) or
character (e.g. "1:3"). If numeric, \code{\link[=get_asp]{get_asp()}} returns the same value
without modification.}

\item{unit}{Units for buffer. Supported options include "meter", "foot",
"kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
instead of "kilometer") are also supported. Distance in units is converted
to units matching GDAL units for x; defaults to "meter"}

\item{crs}{Coordinate reference system of bounding box to return}

\item{trim}{If \code{TRUE}, data is trimmed to area with \link[sf:geos_binary_ops]{sf::st_intersection}.
This option ignores any \code{dist}, \code{diag_ratio}, or \code{asp} parameters. Default
\code{FALSE}.}

\item{crop}{If \code{TRUE}, data is cropped to location or bounding box
\link[sf:st_crop]{sf::st_crop} adjusted by the \code{dist}, \code{diag_ratio}, and \code{asp} parameters
provided. Default TRUE.}

\item{...}{Additional parameters; bbox (used instead of location or adjusted
location), county and state (used with get_counties or get_states), join
(passed to \link[sf:st_join]{sf::st_filter})}
}
\description{
Location can be:
}
\details{
\itemize{
\item A \code{sf}, \code{bbox}, or \code{sfc} object
\item A U.S. state (name, abbreviation, or GeoID) or county (GeoID)
\item An address
}
}
