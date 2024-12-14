
#' Compose a Raster Layer from Multiple Files
#'
#' Combines multiple raster files into a single virtual raster layer (VRT).
#' It accepts one or more folder names containing raster files and creates a
#' virtual raster file.
#' If no output file name is provided, a temporary file is used.
#'
#' @param dir A string or vector of strings representing folder names containing
#'   raster files.
#' @param out_file A string specifying the output file name (without extension).
#'   If `NULL`, a temporary file is used.
#'
#' @return A `SpatRaster` object from the `terra` package.
#'
#' @family transform raster functions
#'
#' @examples
#' input_dir <- system.file("extdata", "mdt", package = "sigugr")
#'
#' r <- compose_raster(input_dir)
#'
#' @export
compose_raster <- function(dir, out_file = NULL) {
  if (!is.character(dir)) {
    stop("'dir' must be a string or a vector of strings.")
  }

  files <- unlist(lapply(dir, list_dir_rasters))

  if (length(files) == 0) {
    stop("No raster files found in the specified directories.")
  }

  if (is.null(out_file)) {
    out_file <- paste0(tempdir(), '/', snakecase::to_snake_case(paste0(Sys.time())))
  }

  vrt <- terra::vrt(files, paste0(out_file, ".vrt"), overwrite = TRUE)
  return(vrt)
}
