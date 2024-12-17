
#' Publish Bands of a Raster to GeoServer
#'
#' Publishes bands of a multi-band GeoTIFF raster file as separate coverages
#' in a specified workspace on a GeoServer instance.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param raster A character string specifying the file path to the GeoTIFF raster file
#'   to be uploaded.
#' @param prefix A string to prepend to each layer name. Default is `NULL`.
#' @param postfix A string to append to each layer name. Default is `NULL`.
#' @param bands A named integer vector, index of the bands to publish with layer names.
#'   If it is `NULL`, which is the default value, all bands are published using the band
#'   name as the layer name. If unnamed indices are provided, the band name is also used
#'   as the layer name.
#'
#' @return An integer:
#' \itemize{
#'   \item \code{0} if the operation was successful or if the layer already exists.
#'   \item \code{1} if an error occurred.
#' }
#'
#' @family publish to GeoServer
#'
#' @examples
#' \dontrun{
#' gso <- geoserver(
#'   url = "http://localhost:8080/geoserver",
#'   user = "admin",
#'   password = "geoserver",
#'   workspace = "sigugr_test"
#' )
#'
#' source_tif <- system.file("extdata/sat.tif", package = "sigugr")
#'
#' gso |>
#'   publish_bands(source_tif)
#' }
#' @export
publish_bands <- function(gso, raster, prefix, postfix, bands)
  UseMethod("publish_bands")


#' @rdname publish_bands
#' @export
publish_bands.geoserver <- function(gso, raster, prefix = NULL, postfix = NULL, bands = NULL) {

  # Check if the raster file exists
  if (!file.exists(raster)) {
    stop("The specified raster file does not exist: ", raster)
  }

  sr <- terra::rast(raster)

  bands <- name_raster_bands(sr, prefix = prefix, postfix = postfix, bands = bands)

  for (band_name in names(bands)) {
    band_index <- bands[band_name]

    # Extract the specific band
    band <- sr[[band_index]]

    temp_file <- tempfile(fileext = ".tif")

    # Save the band to the temporary file
    terra::writeRaster(band, temp_file, overwrite = TRUE)

    result <- publish_raster(gso, temp_file, layer = band_name)

    unlink(temp_file)

    if (result != 0) {
      message("Not all available bands have been published.")
      return(result)
    }
  }

  return(0)
}
