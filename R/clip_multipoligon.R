#' Clip a multipoligon vector layer based on a polygon
#'
#' We will use this function only if function `clip_vector()` gives an error.
#' The error I have found is `ParseException: Unknown WKB type 12`, it has to do
#' with type *MULTIPOLYGON*.
#'
#' The solution adopted is based on the one described in
#' https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12
#'
#' We must ensure that the layer to be cut has the correct encoding.
#'
#' Project the result to the CRS of the clipping polygon.
#'
#' @param vector A `sf` multipoligon vector layer.
#' @param polygon A `sf` polygon layer.
#'
#' @return A `sf` vector layer.
#'
#' @family transformation functions
#'
#' @examples
#' #
#'
#' @export
clip_multipoligon <- function(vector, polygon) {
  tryCatch({
    v <- sf::st_cast(vector, to = "MULTIPOLYGON")
    clip_vector(v, polygon)
  },
  error = function(e) {
    f <- tempfile(fileext = ".gpkg")
    g <- tempfile(fileext = ".gpkg")
    sf::st_write(vector, f, quiet = TRUE)
    gdalUtilities::ogr2ogr(f, g, f = "GPKG", nlt = "MULTIPOLYGON")
    v <- sf::st_read(g, quiet = TRUE)
    clip_vector(v, polygon)
  })
}
