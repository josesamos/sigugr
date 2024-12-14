#' Clip a raster based on a polygon
#'
#' This function clips a raster using a polygon, preserving the coordinate
#' reference system (CRS) of the raster.
#'
#' @param raster A `terra` raster to be clipped.
#' @param polygon A `sf` polygon layer used for clipping.
#'
#' @return A `terra` raster clipped to the extent of the polygon.
#'
#' @family clip functions
#'
#' @examples
#' source_gpkg <- system.file("extdata", "sigugr.gpkg", package = "sigugr")
#' p <-sf::st_read(source_gpkg, layer = 'lanjaron', quiet = TRUE)
#'
#' source_tif <- system.file("extdata", "sat.tif", package = "sigugr")
#' r <- terra::rast(source_tif)
#'
#' result <- clip_raster(r, p)
#'
#' @export
clip_raster <- function(raster, polygon) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input 'raster' must be a 'terra' SpatRaster object.")
  }
  if (!inherits(polygon, "sf")) {
    stop("The input 'polygon' must be an 'sf' polygon object.")
  }

  if (sf::st_crs(polygon)$wkt != terra::crs(raster)) {
    polygon <- sf::st_transform(polygon, terra::crs(raster))
  }

  r <- terra::crop(raster, polygon)
  terra::mask(r, polygon)
}
