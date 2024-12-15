
#' @export
publish_layers <- function(gso, layers)
  UseMethod("publish_layers")


#' @rdname publish_layers
#' @export
publish_layers.geoserver <- function(gso, layers = NULL) {
}

#' Apply a Function to PostGIS Layers with Vector Geometry
#'
#' This function iterates over a set of layers in a PostGIS database, checking whether each layer
#' contains vector geometry. If the layer meets the criteria, the provided function `F` is applied
#' to the layer. If the `layers` parameter is `NULL`, the function will retrieve all layers
#' with vector geometry in the database and process them using the `sf` package.
#'
#' @param conn A valid connection to a PostGIS database (`DBI` connection object).
#' @param layers An optional character vector of layer names to check and process.
#'   If `NULL` (default), all vector geometry layers in the database will be processed.
#' @param F A function to apply to each valid vector geometry layer. The function should accept
#'   the layer name as its argument.
#'
#' @return A named list where each name corresponds to a processed layer, and the values are
#'   the results of applying `F` to each layer.
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mydb", host = "localhost",
#'                        port = 5432, user = "user", password = "password")
#' process_layers_sf(conn, layers = c("layer1", "layer2"), F = function(layer) {
#'   message("Processing layer: ", layer)
#'   return(layer)
#' })
#' }
#'
#' @export
process_layers_sf <- function(conn, layers = NULL, F) {
  # Load necessary package
  library(sf)

  # Retrieve all vector layers if `layers` is NULL
  if (is.null(layers)) {
    message("Fetching all vector layers from the database...")
    db_layers <- st_layers(conn)$name
  } else {
    db_layers <- layers
  }

  # Initialize an empty list to store results
  results <- list()

  # Process each layer
  for (layer in db_layers) {
    tryCatch({
      # Check if the layer can be read as vector geometry
      sf_object <- st_read(conn, layer = layer, quiet = TRUE)
      if (inherits(sf_object, "sf")) {
        # Apply the provided function F
        results[[layer]] <- F(layer)
      } else {
        message("Skipping non-vector layer: ", layer)
      }
    }, error = function(e) {
      message("Error processing layer '", layer, "': ", e$message)
    })
  }

  return(results)
}
