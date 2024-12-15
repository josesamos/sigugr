
#' Publish a Band of a Raster to GeoServer
#'
#' Publishes a band of a multi-band GeoTIFF raster file as a separate coverage
#' in a specified workspace and data store on a GeoServer instance.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param layer A string, the name of the layer to publish.
#' @param title A string, an optional title for the layer. Defaults to the layer
#'   name if not provided.
#' @param band_index The index of the band to publish (1-based).
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
#' }
#' @export
publish_band <- function(gso, layer, title, band_index)
  UseMethod("publish_band")


#' @rdname publish_band
#' @export
publish_band.geoserver <- function(gso, layer, title = NULL, band_index = 1) {
  if (is.null(title)) {
    title <- layer
  }

  # Construct the request URL for checking if the layer exists
  layer_url <- paste0(
    gso$url, "/rest/workspaces/", gso$workspace,
    "/coveragestores/", gso$datastore, "/coverages"
  )
  check_layer_url <- paste0(
    gso$url, "/rest/layers/", gso$workspace, ":", layer_name
  )

  check_response <- httr::GET(
    url = check_layer_url,
    httr::authenticate(gso$user, gso$password)
  )

  if (httr::status_code(check_response) == 200) {
    message("Layer already exists.")
    return(0) # Layer exists, no need to create it
  }


  # Create the request body for the layer
  layer_body <- jsonlite::toJSON(
    list(
      coverage = list(
        name = layer_name,
        title = layer_name,
        nativeCoverageName = paste0("Band", band_index),
        enabled = TRUE
      )
    ), auto_unbox = TRUE
  )

  # Send the POST request to create the coverage
  response <- httr::POST(
    url = layer_url,
    httr::authenticate(gso$user, gso$password),
    body = layer_body,
    encode = "json",
    httr::content_type_json()
  )


  # Check the HTTP response status
  if (httr::status_code(response) == 201) {
    message("Layer published successfully.")
    return(0)
  } else {
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    message("Failed to publish layer. Error: ", error_message)
    return(1)
  }
}
