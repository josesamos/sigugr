#' Publish a Vector Layer Set to GeoServer
#'
#' Publishes a vector layer set to GeoServer. The layer source must have previously
#' been defined as a GeoSever datastore.
#'
#' Iterates over a set of layers in a source database, checking whether each layer
#' contains vector geometry. If the layer meets the criteria, it is published. If the
#' `layers` parameter is `NULL`, the function will publish all layers with vector
#' geometry in the source.
#'
#' Prints an appropriate messages indicating success or failure.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param source A valid connection to a PostGIS database (`RPostgres` connection object).
#' @param layers An optional character vector of layer names to check and publish.
#'   If `NULL` (default), all vector geometry layers in the source will be published.
#'
#' @return An integer:
#' \itemize{
#'   \item \code{0} if the operation was successful for all layers.
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
#' source <- RPostgres::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = 'mydb',
#'   host = 'localhost',
#'   port = '5432',
#'   user = 'user',
#'   password = 'password'
#' )
#'
#' gso |>
#'   publish_layer_set(source)
#' }
#' @export
publish_layer_set <- function(gso, source, layers)
  UseMethod("publish_layer_set")


#' @rdname publish_layer_set
#' @export
publish_layer_set.geoserver <- function(gso, source, layers = NULL) {

  # Retrieve all vector layers if `layers` is NULL
  if (is.null(layers)) {
    message("Fetching all vector layers from the database...")
    query <- "SELECT DISTINCT f_table_name FROM geometry_columns"
    res <- suppressWarnings(sf::st_read(source, query = query))
    db_layers <- res[[1]]
  } else {
    db_layers <- layers
  }

  # Process each layer
  for (layer in db_layers) {
    result <- publish_layer(gso, layer)

    if (result != 0) {
      message("Not all available layers have been published.")
      return(result)
    }
  }
  return(0)
}
