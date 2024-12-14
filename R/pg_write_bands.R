
#' Write Raster Bands to PostGIS
#'
#' Writes each band of a raster (`terra::SpatRaster`) to a specified schema in a
#' PostGIS database. Each band is written as a separate table in the database.
#'
#' Transforms the table name according to the Snake Case convention.
#'
#' @param sr A `terra::SpatRaster` object containing the raster bands to write.
#' @param conn A database connection object to a PostGIS database (e.g., from `RPostgres::dbConnect`).
#' @param schema A string specifying the schema in the PostGIS database where
#'   the raster layers will be stored. Default is `"public"`.
#' @param prefix A string to prepend to each layer name. Default is `NULL`.
#' @param postfix A string to append to each layer name. Default is `NULL`.
#'
#' @return Invisibly returns a character vector of the names of the tables written to PostGIS.
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
#' tables <- pg_write_bands(sr, conn, schema = "geodata", prefix = "example_", postfix = "_raster")
#'
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
pg_write_bands <- function(sr, conn, schema = "public", prefix = NULL, postfix = NULL) {
  if (!inherits(sr, "SpatRaster"))
    stop("`sr` must be a terra::SpatRaster object.")
  if (length(names(sr)) != length(unique(names(sr))))
    stop("The SpatRaster object `sr` must have bands with different names.")

  # Default values for prefix and postfix
  prefix <- as.character(prefix)
  postfix <- as.character(postfix)

  tables <- NULL
  for (band_name in names(sr)) {
    r <- sr[[band_name]]
    table_name <- paste0(prefix, band_name, postfix)
    table_name <- snakecase::to_snake_case(table_name)
    tables <- c(tables, table_name)
    rpostgis::pgWriteRast(conn, c(schema, table_name), raster = r)
  }

  invisible(tables)
}
