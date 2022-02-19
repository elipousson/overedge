#' Convert simple feature object to data frame
#'
#' Helper function to convert a simple feature object to data frame by dropping
#' geometry, converting geometry to well known text, or (if the geometry type is
#' not POINT) getting coordinates for a centroid or point on surface. If an sfc
#' object is provided, sf_to_sfc provides coordinates but the "drop" geometry
#' option is not supported.
#'
#' @param x sf or sfc object
#' @param crs coordinate reference system, Default: 4326
#' @param geometry type of geometry to include in data frame. options include
#'   "drop", "wkt", "centroid", "point", Default: 'centroid'
#' @param coords coordinate columns used if geometry is 'centroid' or 'point';
#'   Default: c("lon", "lat")
#' @param keep_all If FALSE, drop all columns other than those named in coords,
#'   Default: TRUE
#' @return a data frame with geometry dropped or converted to wkt or coordinates
#'   for the centroid or point on surface
#' @seealso
#'  \code{\link[sf]{st_coordinates}}
#' @rdname sf_to_df
#' @export
#' @importFrom sf st_crs st_transform st_as_text st_as_sfc st_geometry_type st_centroid st_point_on_surface st_coordinates st_drop_geometry
#' @importFrom dplyr bind_cols select
#' @importFrom tidyselect all_of
sf_to_df <- function(x,
                     crs = 4326,
                     coords = c("lon", "lat"),
                     geometry = "centroid",
                     keep_all = TRUE) {
  if (!is.null(crs) && (sf::st_crs(crs) != sf::st_crs(x))) {
    x <- sf::st_transform(x, crs)
  }

  geometry <- match.arg(geometry, c("drop", "wkt", "centroid", "point"))
  x_coords <- NULL

  if (geometry == "wkt") {
    # Convert geometry to wkt
    x$wkt <- sf::st_as_text(sf::st_as_sfc(x))
  } else {
    # Convert to coordinates at centroid or as a point on surface
    # FIXME: This approach may be an issue if a sf object has mixed geometry
    geometry_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

    if (geometry_type != "POINT") {
      if (geometry == "centroid") {
        x <- suppressWarnings(sf::st_centroid(x))
      } else if (geometry == "point") {
        x <- suppressWarnings(sf::st_point_on_surface(x))
      }
    }

    x_coords <- as.data.frame(sf::st_coordinates(x))

    # FIXME: This automatic reversal needs to be documented
    if (grepl("lat|Y|y", coords[1])) {
      coords <- rev(coords)
    }

    x_coords <-
      dplyr::select(
        x_coords,
        "{coords[1]}" := X,
        "{coords[2]}" := Y
      )
  }


  if (check_sf(x)) {
    x <-
      dplyr::bind_cols(
        sf::st_drop_geometry(x),
        x_coords
      )
  } else {
    # Returning coordinates for sfc object
    x <- x_coords
    keep_all <- TRUE
  }

  if (!keep_all) {
    x <- dplyr::select(x, tidyselect::all_of(coords))
  }

  return(as.data.frame(x))
}
