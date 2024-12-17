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


#' Name Raster Bands with Prefix and Postfix
#'
#' This function assigns names to the bands of a `SpatRaster` object, optionally
#' adding a prefix and/or postfix to the names. It validates that band indices are
#' within the valid range and ensures that band names are unique.
#'
#' @param sr A `SpatRaster` object created using the `terra` package.
#' @param prefix A character string to add as a prefix to each band name. Default is NULL.
#' @param postfix A character string to add as a postfix to each band name. Default is NULL.
#' @param bands A named integer vector, index of the bands to publish with layer names.
#'   If it is `NULL`, which is the default value, all bands are published using the band
#'   name as the layer name. If unnamed indices are provided, the band name is also used
#'   as the layer name.
#'
#' @return A named vector of band indices with updated names.
#'
#' @keywords internal
#' @noRd
name_raster_bands <- function(sr, prefix = NULL, postfix = NULL, bands = NULL) {

  if (is.null(bands)) {
    # If no bands are specified, process all bands
    bands <- seq_len(terra::nlyr(sr))
    names(bands) <- if (!all(names(sr) == "")) names(sr) else paste0("Band_", seq_len(terra::nlyr(sr)))
  } else {
    # Check if requested band indices are valid
    if (any(bands > terra::nlyr(sr) | bands < 1)) {
      stop("Some band indices are out of bounds. The raster has ", terra::nlyr(sr), " bands.")
    }

    # If bands are unnamed, use the names from the raster or generate defaults
    if (is.null(names(bands))) {
      names(bands) <- if (!all(names(sr) == "")) names(sr)[bands] else paste0("Band_", bands)
    }
  }

  if (length(names(bands)) != length(unique(names(bands))))
    stop("The raster must have bands with different names.")

  # Default values for prefix and postfix
  prefix <- as.character(prefix)
  postfix <- as.character(postfix)

  names(bands) <- paste0(prefix, names(bands), postfix)

  bands
}
