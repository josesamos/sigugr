
#' Publish Each Band of a Raster Separately to GeoServer
#'
#' Publishes each band of a multi-band GeoTIFF raster file as a separate coverage
#' in a specified workspace and data store on a GeoServer instance.
#'
#' @param geoserver_url A character string specifying the base URL of the GeoServer
#'   instance (e.g., "http://localhost:8080/geoserver").
#' @param user A character string for the GeoServer username with publish permissions.
#' @param password A character string for the GeoServer password corresponding to
#'   the specified user.
#' @param workspace A character string specifying the target workspace where the
#'   raster bands will be published.
#' @param data_store A character string specifying the base name of the data store
#'   in GeoServer. Band names will be appended to this base name.
#' @param raster A character string specifying the file path to the GeoTIFF raster
#'   file to be uploaded.
#'
#' @return A named list where the names correspond to the raster bands and the values
#'   are integers indicating the status of the operation for each band:
#'   - `0`: The band was published successfully.
#'   - `1`: An error occurred during the publishing process.
#'
#' @examples
#' \dontrun{
#' geoserver_url <- "http://localhost:8080/geoserver"
#' user <- "admin"
#' password <- "geoserver"
#' workspace <- "example_workspace"
#' data_store <- "example_raster"
#' raster <- "path/to/multiband_raster.tif"
#'
#' gs_publish_bands(
#'   geoserver_url = geoserver_url,
#'   user = user,
#'   password = password,
#'   workspace = workspace,
#'   data_store = data_store,
#'   raster = raster
#' )
#' }
#' @export
gs_publish_bands <- function(geoserver_url,
                             user,
                             password,
                             workspace,
                             data_store,
                             raster) {
  # Load the raster using terra
  raster_obj <- terra::rast(raster)
  num_bands <- terra::nlyr(raster_obj)

  # Initialize results list
  results <- list()

  # Loop through each band
  for (band_index in 1:num_bands) {
    # Extract the individual band
    band <- terra::subset(raster_obj, band_index)

    # Temporary file for single-band raster
    temp_band_file <- tempfile(fileext = ".tif")

    # Save the single-band raster to a temporary file
    terra::writeRaster(band, temp_band_file, overwrite = TRUE)

    # Construct a unique data store name for the band
    band_data_store <- paste0(data_store, "_band", band_index)

    # Publish the single-band raster using gs_publish_raster
    result <- gs_publish_raster(
      geoserver_url = geoserver_url,
      user = user,
      password = password,
      workspace = workspace,
      data_store = band_data_store,
      raster = temp_band_file
    )

    # Store the result in the results list
    results[[paste0("band", band_index)]] <- result

    # Clean up the temporary file
    unlink(temp_band_file)
  }

  return(results)
}
