#' Clip a raster based on a polygon
#'
#' This function clips a raster using a polygon, preserving the coordinate
#' reference system (CRS) of the raster.
#'
#' @param raster A `terra` raster to be clipped.
#' @param polygon A `sf` polygon layer used for clipping.
#' @param keep_crs Logical. If `TRUE`, retains the original CRS of the raster.
#' If `FALSE`, transforms the raster to the polygon CRS. Default is `TRUE`.
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
clip_raster <- function(raster, polygon, keep_crs = TRUE) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input 'raster' must be a 'terra' SpatRaster object.")
  }
  if (!inherits(polygon, "sf")) {
    stop("The input 'polygon' must be an 'sf' polygon object.")
  }

  if (sf::st_crs(polygon)$wkt != terra::crs(raster)) {
    if (keep_crs) {
      polygon <- sf::st_transform(polygon, terra::crs(raster))
    } else {
      # Clip the raster by the projected buffer
      polygon_bbox <- generate_bbox(polygon)
      resolution <- terra::res(raster)[1]
      polygon_bbox <- safe_buffer(polygon_bbox, resolution * 4)
      polygon_bbox <- sf::st_transform(polygon_bbox, terra::crs(raster))
      raster <- terra::crop(raster, polygon_bbox)
      raster <- terra::mask(raster, polygon_bbox)

      # Project the clipped raster to the CRS of the polygon
      raster <- terra::project(raster, sf::st_crs(polygon)$wkt)
    }
  }

  r <- terra::crop(raster, polygon)
  terra::mask(r, polygon)
}




