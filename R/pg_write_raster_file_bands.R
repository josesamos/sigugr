#' Save raster file bands in separate tables in PostGIS
#'
#' @param file A string vector, raster file names.
#' @param conn A connection to a PostGIS database.
#' @param prefix A string, table name prefix to the band name.
#' @param postfix A string, table name postfix to the band name.
#'
#' @return A string vector, raster file names.
#'
#' @family save functions
#'
#' @examples
#' #
#'
#' @export
pg_write_raster_file_bands <-
  function(file,
           conn,
           prefix = NULL,
           postfix = NULL) {
    for (f in file) {
      name <- basename(f)
      name <- gsub(".tif", "", name, ignore.case = TRUE)
      r <- terra::rast(f)
      pg_write_bands(r,
                     conn,
                     prefix = paste0(prefix, name, '_'),
                     postfix = postfix)
    }
    file
  }
