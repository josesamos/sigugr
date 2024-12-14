
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
#' @return The input `terra::SpatRaster` object, returned invisibly.
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
#' pg_write_bands(sr, conn, schema = "geodata", prefix = "example_", postfix = "_raster")
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

  # Iterate over bands
  for (band_name in names(sr)) {
    r <- sr[[band_name]]
    layer <- paste0(prefix, band_name, postfix)
    layer <- snakecase::to_snake_case(layer)

    rpostgis::pgWriteRast(conn, c(schema, layer), raster = r)
  }

  invisible(sr)
}
