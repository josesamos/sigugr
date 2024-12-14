#' Aggregate Rasters in a Folder
#'
#' Aggregates all raster files (`.tif` or `.jp2`) in a specified folder by the
#' given factor and saves the resulting files in an output folder.
#'
#' If the output folder does not exist, it creates it.
#'
#' @param dir A string specifying the input folder containing raster files.
#' @param out_dir A string specifying the output folder where the aggregated
#'   rasters will be saved.
#' @param factor An integer specifying the aggregation factor (default is 2).
#'
#' @return A character vector with the paths to the processed raster files.
#'
#' @examples
#' temp_dir <- tempdir()
#' input_dir <- system.file("extdata", "mdt", package = "sigugr")
#'
#' result_files <- aggregate_rasters(input_dir, temp_dir, factor = 4)
#'
#' @export
aggregate_rasters <- function(dir, out_dir, factor = 2) {
  lf <- list_dir_rasters(dir)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  nexus <- get_nexus(out_dir)

  for (f in lf) {
    file_name <- basename(f)
    file_name_no_ext <- tools::file_path_sans_ext(file_name)
    r1 <- terra::rast(f)
    r2 <- terra::aggregate(r1, fact = factor)
    output_path <- file.path(out_dir, paste0(file_name_no_ext, ".TIF"))
    terra::writeRaster(r2, output_path, filetype = "GTiff", overwrite = TRUE)
  }

  lf
}

