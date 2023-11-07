#' Save raster bands in separate tables in PostGIS
#'
#' @param sr A `SpatRaster` object.
#' @param conn A connection to a PostGIS database.
#' @param prefix A string, table name prefix to the band name.
#' @param postfix A string, table name postfix to the band name.
#'
#' @return A `SpatRaster` object.
#'
#' @family save functions
#'
#' @examples
#' #
#'
#' @export
pg_write_bands <- function(sr, conn, prefix = NULL, postfix = NULL) {
  for (b in names(sr)) {
    r <- sr[[b]]
    layer <- paste0(prefix, b, postfix)
    rpostgis::pgWriteRast(conn, c("public", layer), raster = r)
  }
  sr
}
