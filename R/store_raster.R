
#' Store Raster to PostGIS
#'
#' Stores all bands of a raster (`terra::SpatRaster`) to a specified schema in a
#' PostGIS database. All bands are written in the same table in the database.
#'
#' Transforms the table name according to the Snake Case convention.
#'
#' @param sr A `terra::SpatRaster` object containing the raster bands to write.
#' @param conn A database connection object to a PostGIS database (e.g., from `RPostgres::dbConnect`).
#' @param schema A string specifying the schema in the PostGIS database where
#'   the raster layers will be stored. Default is `"public"`.
#' @param table_name A string, table name.
#'
#' @return Invisibly returns a character vector of the names of the tables written to PostGIS.
#'
#' @family write to PostGIS
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = "mydb",
#'   host = "localhost",
#'   user = "user",
#'   password = "password"
#' )
#'
#' sr <- terra::rast(nrows = 10, ncols = 10, nlyrs = 3, vals = runif(300))
#'
#' tables <- store_raster(sr, conn, schema = "geodata", table_name = "example_raster")
#'
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
store_raster <- function(sr, conn, schema = "public", table_name) {
  if (!inherits(sr, "SpatRaster"))
    stop("`sr` must be a terra::SpatRaster object.")

  table_name <- snakecase::to_snake_case(table_name)
  rpostgis::pgWriteRast(conn, c(schema, table_name), raster = sr)

  invisible(table_name)
}
