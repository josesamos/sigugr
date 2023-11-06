

#' Aggregate rasters in a folder
#'
#' @param dir A string or string vector.
#' @param out_dir A string, output folder.
#' @param factor A integer, aggregation factor.
#'
#' @return A string vector.
#'
#' @export
aggregate_raster <- function(dir, out_dir, factor = 100) {
  lf <-
    list.files(
      path = dir,
      pattern = "*.TIF|.jp2",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  nexus <- get_nexus(out_dir)
  for (f in lf) {
    file_name <- basename(f)
    n <- nchar(file_name)
    name <- substr(file_name, 1, n - 4)
    r1 <- terra::rast(f)
    r2 <- terra::aggregate(r1, fact = factor)
    terra::writeRaster(r2,
                       paste0(out_dir, nexus, file_name, '.TIF'),
                       filetype = "GTiff",
                       overwrite = TRUE)
  }
  lf
}


#' Get nexus
#'
#' Given a name, if it ends in "/" the nexus is the empty string, otherwise it
#' is "/".
#'
#' @param name A string.
#'
#' @return A string.
#'
#' @keywords internal
get_nexus <- function(name) {
  l <- nchar(name)
  c <- substr(name, start = l, stop = l)
  if (c == "/") {
    nexus <- ""
  } else {
    nexus <- "/"
  }
  nexus
}
