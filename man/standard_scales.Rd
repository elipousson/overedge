% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{standard_scales}
\alias{standard_scales}
\title{Standard map, architectural, and engineering scales}
\format{
A data frame with 32 rows and 16 variables:
\describe{
\item{\code{scale}}{Scale name}
\item{\code{standard}}{Standard (USGS, architectural, or engineering)}
\item{\code{series}}{Series name (USGS map scales only)}
\item{\code{actual_ft}}{Scale distance for 1 ft actual.}
\item{\code{actual_ft_unit}}{Unit of scale for 1 ft actual.}
\item{\code{scale_in}}{Actual distance for 1 in scale.}
\item{\code{scale_in_unit}}{Unit of actual distance for 1 in scale.}
\item{\code{scale_in_accuracy}}{Accuracy of 1 in scale (approximate or exact)}
\item{\code{scale_cm}}{Actual distance for 1 cm scale.}
\item{\code{scale_cm_unit}}{Unit of actual distance for 1 cm scale.}
\item{\code{scale_cm_accuracy}}{Accuracy of 1 cm scale (approximate or exact)}
\item{\code{size_latlon}}{Standard size in latitude/longitude}
\item{\code{size_latlon_unit}}{Unit of latitude/longitude size (minutes or degrees)}
\item{\code{area_approx}}{Approximate actual area}
\item{\code{area_approx_unit}}{Approximate area unit}
\item{\code{series_status}}{Series status (select USGS map series are "abandoned")}
}
}
\usage{
standard_scales
}
\description{
Standard map scales derived from USGS 2002 report on map scales
\url{https://pubs.usgs.gov/fs/2002/0015/report.pdf}
}
\details{
Common architectural and engineering scales derived from FEMA guide to using
scales
\url{https://www.usfa.fema.gov/downloads/pdf/nfa/engineer-architect-scales.pdf}
}
\keyword{datasets}
