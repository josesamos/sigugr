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
#' @family styles functions
#'
#' @details
#' The function reads the first style from the `layer_styles` table in the source
#' GeoPackage or PostGIS database. This style is then applied to all layers in the
#' destination GeoPackage.
#'
#' @examples
#' \dontrun{
#' source_gpkg <- "source.gpkg"
#' dest_gpkg <- "destination.gpkg"
#'
#' copy_styles_layer(from = source_gpkg, to = dest_gpkg)
#' }
#' @export
copy_styles_layer <- function(from, to) {
  style <- clc:::read_style_from_source(from)
  clc:::assign_styles_to_layers(style, to)
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
#' @return The updated style object (`obj`), returned invisibly.
#'
#' @family styles functions
#'
#' @details
#' The function reads the first style from the `layer_styles` table in the source
#' and applies it to the specified layers in the destination PostGIS database.
#'
#' @examples
#' \dontrun{
#' source_gpkg <- "source.gpkg"
#' conn <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = "mydb",
#'   host = "localhost",
#'   user = "user",
#'   password = "password"
#' )
#'
#' layers_to_style <- c("layer1", "layer2")
#'
#' copy_styles_layer_names(
#'   from = source_gpkg,
#'   to = conn,
#'   layers = layers_to_style,
#'   database = "mydb",
#'   schema = "public"
#' )
#'
#' DBI::dbDisconnect(conn)
#' }
#' @export
copy_styles_layer_names <- function(from, to, layers, database, schema = 'public') {
  style <- clc:::read_style_from_source(from)
  clc:::assign_styles_to_layers(style, to, database, schema, layers)
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
#' @param r_clc A `terra` raster object containing the raster values to filter the categories.
#'
#' @return A data frame containing the filtered categories with the following columns:
#'   - `id`: The category ID (integer).
#'   - `description`: The description of the category (character).
#'   - `color`: The color associated with the category in hexadecimal format (character).
#'
#' @family styles functions
#'
#' @details
#' The function retrieves the style definitions from the `layer_styles` table in
#' the provided GeoPackage or PostGIS database. It filters the categories to include
#' only those whose IDs match the unique values present in the raster.
#'
#' This function is useful for associating raster values with their corresponding
#' descriptions and colors, typically for visualization or analysis tasks.
#'
#' @examples
#' \dontrun{
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#' r_clc <- terra::rast("clc_raster.tif")
#'
#' categories <- get_layer_categories(from = gpkg_path, r_clc = r_clc)
#' }
#' @export
get_layer_categories <- function(from, r_clc) {
  style <- clc:::read_style_from_source(from)
  values <- sort(terra::unique(r_clc)[, 1])

  cat <- clc:::extract_categories_and_colors(style)
  cat <- cat[cat$id %in% values, ]
  cat
}
