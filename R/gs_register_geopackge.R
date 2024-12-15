#' Register a GeoPackage as a DataStore in GeoServer
#'
#' This function registers a GeoPackage as a `datastore` in a specified GeoServer workspace
#' using the REST API.
#'
#' If the `datastore` has already been registered previously, there is no need to specify the
#' GeoPackage. For subsequent operations, that `datastore` will be used.
#'
#' In any case, prints an appropriate message.
#'
#' @param An object of class `geoserver`.
#' @param datastore A character string. The name of the datastore to be created.
#' @param geopackage A character string specifying the full file path to the GeoPackage
#'   file. The default is `NULL`, which assumes the datastore has already been registered.
#'
#' @return An object of class `geoserver` or NULL if an error occurred.
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
#' source_gpkg <- system.file("extdata/sigugr.gpkg", package = "sigugr")
#'
#' gso <- gso |>
#'   register_datastore_geopackage("sigugr-geopackage", source_gpkg)
#' }
#'
#' @export
register_datastore_geopackage <- function(gso, datastore, geopackage)
  UseMethod("register_datastore_geopackage")


#' @rdname register_datastore_geopackage
#' @export
register_datastore_geopackage.geoserver <- function(gso, datastore, geopackage = NULL) {
  gso$datastore <- datastore

  # Define URLs
  datastore_url <- paste0(gso$url, "/rest/workspaces/", gso$workspace, "/datastores")
  datastore_check_url <- paste0(datastore_url, "/", datastore)

  # Check if the datastore already exists
  check_response <- httr::GET(url = datastore_check_url, httr::authenticate(gso$user, gso$password))

  if (httr::status_code(check_response) == 200) {
    message("Datastore already exists.")
    return(gso)
  }

  # Prepare the body for datastore creation
  datastore_body <- jsonlite::toJSON(list(
    dataStore = list(
      name = datastore,
      type = "GeoPackage",
      connectionParameters = list(entry = list(
        list(key = "database", value = geopackage)
      ))
    )
  ), auto_unbox = TRUE)

  # Register the GeoPackage
  response <- httr::POST(
    url = datastore_url,
    httr::authenticate(gso$user, gso$password),
    body = datastore_body,
    encode = "json",
    httr::content_type_json()
  )

  if (httr::status_code(response) == 201) {
    message("GeoPackage successfully registered as a datastore!")
    return(gso)
  } else {
    message("Error registering the GeoPackage: ",
            httr::content(response, "text"))
    return(NULL)
  }
}
