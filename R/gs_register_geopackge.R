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
#' @param gso An object of class `geoserver` containing GeoServer connection details.
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
  # Check if geopackage file exists
  if (!file.exists(geopackage)) {
    stop("The provided GeoPackage file path does not exist.")
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

  register_datastore(gso, datastore, datastore_body)
}
