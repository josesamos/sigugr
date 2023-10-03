#' Compose a raster layer from multiple files
#'
#' The folder name or vector of folder names that contain the files is indicated.
#'
#' We can also indicate the name (with the path and without the extension) of the
#' virtual raster file; if none is indicated, a temporary one is used.
#'
#' @param dir A string or string vector, folder names.
#' @param out_file A string, output file name (without extension).
#'
#' @return A `terra` raster layer.
#'
#' @family generation functions
#'
#' @examples
#' #
#'
#' @export
compose_raster <- function(dir, out_file = NULL) {
  files <- NULL
  for (d in dir) {
    lf <-
      list.files(
        path = d,
        pattern = "*.TIF|*.jp2",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    files <- c(files, lf)
  }
  if (is.null(out_file)) {
    out_file <-
      paste0(tempdir(), '/', snakecase::to_snake_case(paste0(Sys.time())))
  }
  terra::vrt(files, paste0(out_file, ".vrt"), overwrite = TRUE)
}
