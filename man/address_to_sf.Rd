% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_to_df.R
\name{address_to_sf}
\alias{address_to_sf}
\title{Use tidygeocoder to convert an address or data frame with an address column
to an sf object}
\usage{
address_to_sf(
  x,
  address = "address",
  coords = c("lon", "lat"),
  remove_coords = FALSE,
  crs = NULL,
  ...
)
}
\arguments{
\item{x}{Data frame with an address column. Multiple address columns are not currently supported.}

\item{address}{Address column name, Default: 'address'}

\item{coords}{Coordinate columns for input dataframe or output sf object (if
geometry is 'centroid' or 'point') Default: c("lon", "lat").}

\item{remove_coords}{For \link{df_to_sf}, if \code{TRUE}, remove the coordinate columns
after converting a data frame to simple feature object; defaults to
\code{FALSE}.}

\item{crs}{Cordinate reference system to return, Default: 4326 for \link{sf_to_df}
and NULL for \link{df_to_sf}}

\item{...}{Additional parameters passed to \link[tidygeocoder:geo]{tidygeocoder::geo} or \link[tidygeocoder:geocode]{tidygeocoder::geocode}}
}
\value{
A \code{sf} object with POINT geometry for all geocoded addresses with valid coordinates.
}
\description{
Wraps \link[tidygeocoder:geo]{tidygeocoder::geo} and \link[tidygeocoder:geocode]{tidygeocoder::geocode} to convert a character
string or a data frame with an address column.
}
\seealso{
\code{\link[tidygeocoder]{geo}}, \code{\link[tidygeocoder]{geocode}}
}
