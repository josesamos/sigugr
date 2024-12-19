
#' Store Raster to PostGIS
#'
#' Stores all bands of a raster to a specified schema in a PostGIS database.
#' All bands are written in the same table in the database.
#'
#' Transforms the table name according to the Snake Case convention.
#'
#' @param raster A character string specifying the file path to the GeoTIFF raster file
#'   to be stored.
#' @param conn A database connection object to a PostGIS database (e.g., from `RPostgres::dbConnect`).
#' @param schema A string specifying the schema in the PostGIS database where
#'   the raster layers will be stored. Default is `"public"`.
#' @param table_name A string, table name. If it is `NULL`, which is the default value,
#'   the layer name is derived from the filename.
#'
#' @return Invisibly returns a character vector of the names of the tables written to PostGIS.
#'
#' @family write to PostGIS
#'
#' @examples
#' \dontrun{
#' source_tif <- system.file("extdata", "mdt.tif", package = "clc")
#'
#' conn <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = "mydb",
#'   host = "localhost",
#'   user = "user",
#'   password = "password"
#' )
#'
#' tables <- store_raster(source_tif, conn, table_name = "mdt")
#'
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
store_raster <- function(raster, conn, schema = "public", table_name = NULL) {

  # Check if the raster file exists
  if (!file.exists(raster)) {
    stop("The specified raster file does not exist: ", raster)
  }

  if (is.null(table_name)) {
    table_name <- tools::file_path_sans_ext(basename(raster))
  }

  sr <- terra::rast(raster)

  table_name <- snakecase::to_snake_case(table_name)
  rpostgis::pgWriteRast(conn, c(schema, table_name), raster = sr, overwrite = TRUE)

  invisible(table_name)
}
