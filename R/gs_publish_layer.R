
#' Publish a Vector Layer to GeoServer
#'
#' Publishes a vector layer to GeoServer using the REST API.
#'
#' Prints an appropriate message indicating success or failure.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param layer A string, the name of the layer to publish.
#' @param title A string, an optional title for the layer. Defaults to the layer
#'   name if not provided.
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
#' gso <- gso |>
#'   register_datastore_postgis(
#'     "sigugr-postgis",
#'     db_name = 'sigugr_example',
#'     host = 'localhost',
#'     port = 5432,
#'     db_user = 'user',
#'     db_password = 'password',
#'     schema = "public"
#'   )
#'
#' gso |>
#'   publish_layer(layer = 'sigugr_layer')
#'
#' }
#' @export
publish_layer <- function(gso, layer, title)
  UseMethod("publish_layer")


#' @rdname publish_layer
#' @export
publish_layer.geoserver <- function(gso, layer, title = NULL) {
  if (is.null(title)) {
    title <- layer
  }

  # Define URLs
  layer_url <- paste0(gso$url, "/rest/layers/", gso$workspace, ":", layer)
  featuretype_url <- paste0(gso$url,
                            "/rest/workspaces/",
                            gso$workspace,
                            "/datastores/",
                            gso$datastore,
                            "/featuretypes")

  # Check if the layer already exists
  check_response <- httr::GET(
    url = layer_url,
    httr::authenticate(gso$user, gso$password)
  )

  if (httr::status_code(check_response) == 200) {
    message(sprintf("Layer %s already exists.", layer))
    return(0)
  }

  # Prepare layer configuration as JSON
  layer_config <- list(featureType = list(
    name = layer,
    nativeName = layer,
    title = title
  ))
  layer_config_json <- jsonlite::toJSON(layer_config, auto_unbox = TRUE)

  # Send POST request to GeoServer
  response <- httr::POST(
    featuretype_url,
    httr::authenticate(gso$user, gso$password),
    httr::add_headers("Content-Type" = "application/json"),
    body = layer_config_json,
    encode = "json"
  )

  # Check the HTTP response status
  if (httr::status_code(response) == 201) {
    message(sprintf("Layer %s published successfully.", layer))
    return(0)
  } else {
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    message(sprintf("Failed to publish layer %s. Error: ", layer), error_message)
    return(1)
  }
}
