% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_dist_units.R
\name{is_dist_units}
\alias{is_dist_units}
\alias{diff_dist}
\alias{is_diff_dist}
\alias{is_same_dist}
\alias{is_longer}
\alias{is_shorter}
\alias{get_dist_units}
\alias{as_dist_units}
\alias{is_diff_area}
\alias{is_same_area}
\title{General utility functions for working with distance units objects}
\usage{
is_dist_units(x)

is_diff_dist(x, y, units = NULL)

is_same_dist(x, y, dist = NULL, diff = FALSE, ...)

is_longer(x, y)

is_shorter(x, y)

get_dist_units(x, null.ok = TRUE)

as_dist_units(x, units = NULL, null.ok = FALSE)

is_diff_area(x, y, units = NULL, union = TRUE)

is_same_area(x, y, units = NULL, union = TRUE, diff = FALSE, ...)
}
\arguments{
\item{x, y}{objects to check}

\item{units}{For \link{is_diff_dist}, if x and y are both not units objects, use
units; default to \code{NULL}.}

\item{dist}{type of distance to compare if x and y are \code{sf}, \code{sfc}, or \code{bbox}
objects; "diagdist", "xdist", "ydist". defaults to \code{NULL}.}

\item{diff}{If \code{TRUE}, return results from \link{is_diff_dist} or \link{is_diff_area};
if \code{FALSE}, return logical indicator; defaults to \code{FALSE}}

\item{...}{Additional parameters passed to all.equal}

\item{null.ok}{If null.ok is \code{TRUE}, allow x to return a \code{NULL} value; if
\code{FALSE}, error on \code{NULL} values.}

\item{union}{If \code{TRUE}, union objects before comparing area with
\code{\link[=is_diff_area]{is_diff_area()}} or \code{\link[=is_same_area]{is_same_area()}}, defaults to \code{TRUE}.}
}
\description{
\itemize{
\item \link{is_dist_units}: Is x a distance unit object?
\item \link{is_diff_dist}: What is the difference between x and y distance?
\item \link{is_same_dist}: Is x the same distance as y? or does the bbox of x and bbox of y have the same x, y, or diagonal distance?
\item \link{is_shorter}, is_longer: Is x shorter or longer than y?
\item \link{is_same_area}: do x and y have the same area?
}
}
\details{
There are two additional functions that support these utility functions:
\itemize{
\item \link{get_dist_units}: Get the distance units from x (if x is a sf or units
objects or a character string from \link{dist_unit_options})
\item \link{as_dist_units}: Convert x to units using \link[units:units]{units::as_units}
}
}
\seealso{
Other dist: 
\code{\link{convert_dist_scale}()},
\code{\link{convert_dist_units}()},
\code{\link{get_measurements}},
\code{\link{sf_bbox_misc}}
}
\concept{dist}
