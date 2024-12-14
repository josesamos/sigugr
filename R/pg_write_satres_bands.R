#' Save `satres` bands in separate tables in PostGIS
#'
#' @param sr A `satres` object.
#' @param conn A connection to a PostGIS database.
#' @param prefix A string, table name prefix to the resolution names.
#' @param postfix A string, table name postfix to the resolution names.
#'
#' @return A `satres` object.
#'
#' @examples
#' #
#'
#' @export
pg_write_satres_bands <- function(sr, conn, prefix = NULL, postfix = NULL) {
  for (res in names(sr$bands)) {
    for (b in names(sr$bands[[res]])) {
      r <- sr$bands[[res]][[b]]
      layer <- paste0(prefix, res, "_", b, postfix)
      rpostgis::pgWriteRast(conn, c("public", layer), raster = r)
    }
  }
  sr
}
