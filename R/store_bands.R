
#' Store Raster Bands to PostGIS
#'
#' Stores each band of a raster to a specified schema in a PostGIS database. Each
#' band is written as a separate table in the database.
#'
#' Transforms the table name according to the Snake Case convention.
#'
#' @param raster A character string specifying the file path to the GeoTIFF file
#'   containing the raster bands to be stored.
#' @param conn A database connection object to a PostGIS database (e.g., from `RPostgres::dbConnect`).
#' @param schema A string specifying the schema in the PostGIS database where
#'   the raster layers will be stored. Default is `"public"`.
#' @param prefix A string to prepend to each layer name. Default is `NULL`.
#' @param postfix A string to append to each layer name. Default is `NULL`.
#' @param bands A named integer vector, index of the bands to store with layer names.
#'   If it is `NULL`, which is the default value, all bands are stored using the band
#'   name as the layer name. If unnamed indices are provided, the band name is also used
#'   as the layer name.
#'
#' @family write to PostGIS
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
#' sr_file <- tempfile(fileext = ".tif")
#' terra::writeRaster(sr, sr_file, filetype = "GTiff", overwrite = TRUE)
#'
#' tables <- store_bands(sr_file, conn, schema = "geodata", prefix = "example_", postfix = "_raster")
#'
#' DBI::dbDisconnect(conn)
#' }
#'
#' @export
store_bands <- function(raster, conn, schema = "public", prefix = NULL, postfix = NULL, bands = NULL) {

  # Check if the raster file exists
  if (!file.exists(raster)) {
    stop("The specified raster file does not exist: ", raster)
  }

  sr <- terra::rast(raster)

  bands <- name_raster_bands(sr, prefix = prefix, postfix = postfix, bands = bands)

  tables <- NULL
  for (band_name in names(bands)) {
    band_index <- bands[band_name]

    # Extract the specific band
    band <- sr[[band_index]]

    table_name <- snakecase::to_snake_case(band_name)
    tables <- c(tables, table_name)
    rpostgis::pgWriteRast(conn, c(schema, table_name), raster = band)
  }

  invisible(tables)
}
