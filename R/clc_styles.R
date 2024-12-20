#' Copy Styles from a Source to a Destination
#'
#' Copies layer styles from a source (GeoPackage or PostGIS database) to a destination
#' (GeoPackage or PostGIS database). The source and destination can be specified flexibly,
#' and the function supports copying styles to multiple layers in the destination.
#'
#' @param from A data source for the input style. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param from_layer Character (optional). Name of the layer in the source to copy the style from.
#' If not provided, the function will use the first layer in the source with a defined style.
#' @param to A data destination for the output styles. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param database Character (optional). Name of the destination PostGIS database
#'   (required if the destination is a PostGIS connection object).
#' @param schema Character. Schema in the destination PostGIS database where the styles will be applied.
#'   Default is "public".
#' @param to_layers Character vector (optional). Names of the layers in the destination where the style
#'   will be applied. If not provided, the style will be applied to all layers in the destination.
#'
#' @return The updated `layer_styles` table, returned invisibly.
#'
#' @family style functions
#'
#' @examples
#' # Ex1:
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' layer_data <- sf::st_read(source_gpkg, layer = "clc", quiet = TRUE)
#'
#' dest_gpkg <- tempfile(fileext = ".gpkg")
#' sf::st_write(layer_data, dest_gpkg, layer = "clc", quiet = TRUE)
#'
#' copy_styles(from = source_gpkg, to = dest_gpkg)
#'
#' \dontrun{
#' # Ex2:
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' conn <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = "mydb",
#'   host = "localhost",
#'   user = "user",
#'   password = "password"
#' )
#'
#' copy_styles(
#'   from = source_gpkg,
#'   to = conn,
#'   database = "mydb",
#'   schema = "public",
#'   to_layers = c("layer1", "layer2"),
#' )
#'
#' DBI::dbDisconnect(conn)
#' }
#' @export
copy_styles <- function(from, from_layer = NULL, to, database = NULL, schema = 'public', to_layers = NULL) {
  style <- clc:::read_style_from_source(from, from_layer)
  clc:::assign_styles_to_layers(style, to, database, schema, to_layers)
}


#' Copy Layer Styles from Source to Destination in GeoPackage
#'
#' Copies the first style definition from a source (either a GeoPackage file or a
#' PostGIS connection) and assigns it to all layers in the destination GeoPackage.
#'
#' @param from A data origin. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param to A string representing the path to the destination GeoPackage file.
#'
#' @return The updated `layer_styles` table, returned invisibly.
#'
#' @family style functions
#'
#' @details
#' The function reads the first style from the `layer_styles` table in the source
#' GeoPackage or PostGIS database. This style is then applied to all layers in the
#' destination GeoPackage.
#'
#' @examples
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#' layer_data <- sf::st_read(source_gpkg, layer = "clc", quiet = TRUE)
#'
#' dest_gpkg <- tempfile(fileext = ".gpkg")
#' sf::st_write(layer_data, dest_gpkg, layer = "clc", quiet = TRUE)
#'
#' copy_styles_layer(from = source_gpkg, to = dest_gpkg)
#' @keywords internal
#' @noRd
copy_styles_layer <- function(from, to) {
  copy_styles(from = from, to = to)
}


#' Copy Layer Styles from Source to Specific Layers in PostGIS Database
#'
#' Copies the first style definition from a source (either a GeoPackage file or
#' a PostGIS connection) to the specified layers in a PostGIS database.
#'
#' @param from A data origin. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param to A database connection object to the destination PostGIS database
#'   (e.g., from `RPostgres::dbConnect`).
#' @param layers A character vector of layer names in the destination database
#'   to which the style should be applied.
#' @param database A string specifying the name of the PostGIS database.
#' @param schema A string specifying the schema in the PostGIS database where
#'   the layers reside. Default is `"public"`.
#'
#' @return The updated `layer_styles` table, returned invisibly.
#'
#' @family style functions
#'
#' @details
#' The function reads the first style from the `layer_styles` table in the source
#' and applies it to the specified layers in the destination PostGIS database.
#'
#' @examples
#' \dontrun{
#' source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
#'
#' conn <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = "mydb",
#'   host = "localhost",
#'   user = "user",
#'   password = "password"
#' )
#'
#' copy_styles_layer_names(
#'   from = source_gpkg,
#'   to = conn,
#'   layers = c("layer1", "layer2"),
#'   database = "mydb",
#'   schema = "public"
#' )
#'
#' DBI::dbDisconnect(conn)
#' }
#' @keywords internal
#' @noRd
copy_styles_layer_names <- function(from, to, layers, database, schema = 'public') {
  copy_styles(from = from, to = to, database = database, schema = schema, to_layers = layers)
}


#' Get Layer Categories Based on Raster Values
#'
#' Extracts the categories (IDs, descriptions, and colors) from the first style
#' definition stored in a GeoPackage or PostGIS database. The extracted categories
#' are filtered to include only those present in the raster values.
#'
#' @param from A data origin. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param r_clc A `terra` raster object containing the raster values to filter the categories. If NULL,
#'   returns all categories.
#'
#' @return A data frame containing the filtered categories with the following columns:
#'   - `id`: The category ID (integer).
#'   - `description`: The description of the category (character).
#'   - `color`: The color associated with the category in hexadecimal format (character).
#'
#' @family style functions
#'
#' @details
#' The function retrieves the style definitions from the `layer_styles` table in
#' the provided GeoPackage or PostGIS database. It filters the categories to include
#' only those whose IDs match the unique values present in the raster.
#'
#' It is useful for associating raster values with their corresponding descriptions
#' and colors, typically for visualization or analysis tasks.
#'
#' @examples
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#'
#' categories <- get_layer_categories(from = gpkg_path)
#' @export
get_layer_categories <- function(from, r_clc = NULL) {
  style <- clc:::read_style_from_source(from)

  cat <- clc:::extract_categories_and_colors(style)

  if (!is.null(r_clc)) {
    values <- sort(terra::unique(r_clc)[, 1])
    cat <- cat[cat$id %in% values, ]
  }
  cat
}
