#' Publish a Raster to GeoServer
#'
#' This function publishes a GeoTIFF raster file to a specified workspace and data store on a GeoServer instance.
#'
#' @param geoserver_url A character string specifying the base URL of the GeoServer instance (e.g., "http://localhost:8080/geoserver").
#' @param user A character string for the GeoServer username with publish permissions.
#' @param password A character string for the GeoServer password corresponding to the specified user.
#' @param workspace A character string specifying the target workspace where the raster will be published.
#' @param data_store A character string specifying the name of the target data store in GeoServer.
#' @param raster A character string specifying the file path to the GeoTIFF raster file to be uploaded.
#'
#' @return An integer indicating the status of the operation:
#'   - `0`: The raster was published successfully.
#'   - `1`: An error occurred during the publishing process.
#'
#' @family publish to GeoServer
#'
#' @details
#' The function uses the GeoServer REST API to upload a GeoTIFF raster file to a specified workspace and data store.
#' The raster is sent using an HTTP `PUT` request to the GeoServer endpoint, and the response is checked for success.
#' A message is printed to indicate whether the operation succeeded or failed, and any error messages from GeoServer are displayed.
#'
#' @examples
#' \dontrun{
#' # Publish a raster to GeoServer
#' geoserver_url <- "http://localhost:8080/geoserver"
#' user <- "admin"
#' password <- "geoserver"
#' workspace <- "example_workspace"
#' data_store <- "example_datastore"
#' raster <- "path/to/raster.tif"
#'
#' gs_publish_raster(
#'   geoserver_url = geoserver_url,
#'   user = user,
#'   password = password,
#'   workspace = workspace,
#'   data_store = data_store,
#'   raster = raster
#' )
#' }
#' @export
gs_publish_raster <- function(geoserver_url,
                              user,
                              password,
                              workspace,
                              data_store,
                              raster) {
  url <- paste0(
    geoserver_url,
    "/rest/workspaces/",
    workspace,
    "/coveragestores/",
    data_store,
    "/file.geotiff"
  )

  response <- httr::PUT(
    url,
    httr::authenticate(user, password),
    httr::add_headers("Content-Type" = "image/tiff"),
    body = httr::upload_file(raster)
  )

  # Check the HTTP response status
  if (httr::status_code(response) == 201) {
    message("Raster published successfully.")
    return(0)
  } else {
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    message("Failed to publish raster. Error: ", error_message)
    return(1)
  }
}
