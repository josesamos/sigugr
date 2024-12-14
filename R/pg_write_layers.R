#' Write GeoPackage Layers with Geometry to PostGIS
#'
#' Transfers vector layers with valid geometries from a GeoPackage file
#' to a specified PostGIS database schema. Optionally allows setting a custom
#' geometry column name, adding prefixes or postfixes to the table names, and
#' renaming the layer fields to follow the Snake Case convention.
#'
#' @param gpkg A string, the path to the GeoPackage file.
#' @param layers A string vector, the name of the layers to transfer. If NULL,
#'   all vector layers are transferred.
#' @param conn A PostGIS database connection object created with [RPostgres::dbConnect()].
#' @param schema A string, the schema in PostGIS where layers will be stored. Default is `"public"`.
#' @param prefix A string, an optional prefix to add to the table names in PostGIS. Default is `NULL`.
#' @param postfix A string, an optional postfix to add to the table names in PostGIS. Default is `NULL`.
#' @param geom_colum A string, the name of the geometry column to set. Default is `"geom"`.
#' @param snake_case_fields A logical, whether to convert field names to Snake Case. Default is `TRUE`.
#'
#' @return Invisibly returns a character vector of the names of the tables written to PostGIS.
#'
#' @family write to PostGIS
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mydb")
#' gpkg_file <- "example.gpkg"
#' layers <- NULL
#' pg_write_layers(
#'   gpkg_file, layers, conn, schema = "my_schema", prefix = "pre_", postfix = "_post"
#' )
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
pg_write_layers <- function(gpkg,
                            layers = NULL,
                            conn,
                            schema = "public",
                            prefix = NULL,
                            postfix = NULL,
                            geom_colum = "geom",
                            snake_case_fields = TRUE) {
  if (!file.exists(gpkg)) {
    stop("The GeoPackage file does not exist.")
  }

  # Get the layer names and check for geometry
  layers_info <- sf::st_layers(gpkg)
  geom_layers <- layers_info$name[layers_info$geomtype != ""]
  if (is.null(layers)) {
    layers <- geom_layers
  } else {
    layers <- intersect(layers, geom_layers)
  }

  if (is.na(layers) | length(layers) == 0) {
    stop("No layers with valid geometries found in the GeoPackage.")
  }

  tables <- NULL
  for (layer_name in layers) {
    layer <- sf::st_read(gpkg, layer = layer_name, quiet = TRUE)
    if (snake_case_fields) {
      geom_colum <- snakecase::to_snake_case(geom_colum)
    }
    sf::st_set_geometry(layer, geom_colum)

    # Optionally convert field names to Snake Case
    if (snake_case_fields) {
      names(layer) <- snakecase::to_snake_case(names(layer))
    }

    table_name <- paste0(prefix, layer_name, postfix)
    table_name <- snakecase::to_snake_case(table_name)
    tables <- c(tables, table_name)

    sf::st_write(layer,
                 conn,
                 table = paste0(schema, ".", table_name),
                 overwrite = TRUE)
  }

  invisible(tables)
}
