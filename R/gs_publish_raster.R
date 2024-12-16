#' Publish a Raster to GeoServer
#'
#' This function publishes a GeoTIFF raster file to a workspace and data store on a
#' GeoServer instance.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param raster A character string specifying the file path to the GeoTIFF raster file
#'   to be uploaded.
#' @param layer A string, the name of the layer to publish. If it is `NULL`, which is
#'   the default value, the layer name is derived from the filename.
#'
#' @return An integer:
#' \itemize{
#'   \item \code{0} if the operation was successful or if the layer already exists.
#'   \item \code{1} if an error occurred.
#' }
#'
#' @family publish to GeoServer
#'
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
#'   publish_raster(source_tif, "sat-tiff")
#' }
#' @export
#' @export
publish_raster <- function(gso, raster, layer)
  UseMethod("publish_raster")


#' @rdname publish_raster
#' @export
publish_raster.geoserver <- function(gso, raster, layer = NULL) {

  # Check if the raster file exists
  if (!file.exists(raster)) {
    stop("The specified raster file does not exist: ", raster)
  }

  if (is.null(layer)) {
    layer <- tools::file_path_sans_ext(basename(raster))
  }

  url <- paste0(
    gso$url,
    "/rest/workspaces/",
    gso$workspace,
    "/coveragestores/",
    layer,
    "/file.geotiff"
  )

  response <- httr::PUT(
    url,
    httr::authenticate(gso$user, gso$password),
    httr::add_headers("Content-Type" = "image/tiff"),
    body = httr::upload_file(raster)
  )

  # Check the HTTP response status
  if (httr::status_code(response) == 201) {
    message(sprintf("Raster %s published successfully.", layer))
    return(0)
  } else {
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    message(sprintf("Failed to publish raster %s. Error: ", layer), error_message)
    return(1)
  }
}
