
#' Publish a layer to GeoServer
#'
#' Publishes a vector layer to GeoServer using the REST API. Returns a status code
#' and prints an appropriate message indicating success or failure.
#'
#' @param geoserver_url A string, the base URL of the GeoServer instance.
#' @param user A string, GeoServer username.
#' @param password A string, GeoServer password.
#' @param workspace A string, the name of the GeoServer workspace.
#' @param data_store A string, the name of the data store where the layer resides.
#' @param layer_name A string, the name of the layer to publish.
#' @param title A string, an optional title for the layer. Defaults to the layer
#'   name if not provided.
#'
#' @return Integer. Returns `0` on success or `1` on failure. Prints a message
#'   describing the result.
#'
#' @examples
#' \dontrun{
#' gs_publish_layer(
#'   geoserver_url = "http://my-geoserver.com/geoserver",
#'   user = "admin",
#'   password = "mypassword",
#'   workspace = "my_workspace",
#'   data_store = "my_datastore",
#'   layer_name = "my_layer",
#'   title = "My Layer Title"
#' )
#' }
#' @export
gs_publish_layer <- function(geoserver_url,
                             user,
                             password,
                             workspace,
                             data_store,
                             layer_name,
                             title = NULL) {
  if (is.null(title)) {
    title <- layer_name
  }

  # Prepare layer configuration as JSON
  layer_config <- list(featureType = list(
    name = layer_name,
    nativeName = layer_name,
    title = title
  ))
  layer_config_json <- jsonlite::toJSON(layer_config, auto_unbox = TRUE)

  # Construct the GeoServer REST API URL
  url <- paste0(
    geoserver_url,
    "/rest/workspaces/",
    workspace,
    "/datastores/",
    data_store,
    "/featuretypes"
  )

  # Send POST request to GeoServer
  response <- httr::POST(
    url,
    httr::authenticate(user, password),
    httr::add_headers("Content-Type" = "application/json"),
    body = layer_config_json,
    encode = "json"
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
