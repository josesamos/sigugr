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
