#' Clip a raster layer based on a polygon
#'
#' The result preserves the CRS of the raster.
#'
#' @param raster A `terra` raster layer.
#' @param polygon A `sf` polygon layer.
#'
#' @return A `terra` raster layer.
#'
#' @family transformation functions
#'
#' @examples
#' #
#'
#' @export
clip_raster <- function(raster, polygon) {
  s <- sf::st_transform(polygon, terra::crs(raster))
  r <- terra::crop(raster, s)
  terra::mask(r, s)
}
