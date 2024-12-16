#' Get nexus
#'
#' Determines the appropriate path separator based on whether a folder path
#' ends with a trailing slash or not.
#'
#' @param name A string representing a folder path.
#'
#' @return A string. If the input ends with "/", it returns an empty string.
#'   Otherwise, it returns "/".
#'
#' @keywords internal
#' @noRd
get_nexus <- function(name) {
  if (endsWith(name, "/")) {
    ""
  } else {
    "/"
  }
}


#' List Raster Files in a Directory
#'
#' This function scans a directory and recursively lists all raster files
#' with extensions `.tif` or `.jp2`. The search is case-insensitive.
#'
#' @param dir A character string specifying the path to the directory to search.
#'
#' @return A character vector containing the full paths of all matching files.
#' If no matching files are found, an empty character vector is returned.
#'
#' @keywords internal
#' @noRd
list_dir_rasters <- function(dir) {
  lf <- list.files(
    path = dir,
    pattern = "\\.(tif|jp2)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  lf
}


#' Get a project crs
#'
#' @param layer A `sf` object.
#'
#' @return An integer, CRS code.
#'
#' @keywords internal
#' @noRd
get_projected_crs <- function(layer) {
  # Check if the CRS is geographic (degrees)
  if (sf::st_is_longlat(layer)) {
    # Calculate the centroid of the layer
    centroid <- sf::st_centroid(sf::st_union(layer))
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1]
    lat <- coords[2]

    # Determine the UTM zone based on longitude
    utm_zone <- floor((lon + 180) / 6) + 1

    # Select EPSG code for the appropriate UTM zone
    epsg_utm <- if (lat >= 0) {
      32600 + utm_zone  # Northern Hemisphere
    } else {
      32700 + utm_zone  # Southern Hemisphere
    }
    crs <- epsg_utm
  } else {
    crs <- sf::st_crs(layer)$epsg
  }
  crs
}
