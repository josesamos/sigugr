#' Clip a raster layer based on a polygon
#'
#' This function clips a raster layer using a polygon, preserving the coordinate reference system (CRS) of the raster.
#'
#' @param raster A `terra` raster layer to be clipped.
#' @param polygon A `sf` polygon layer used for clipping.
#'
#' @return A `terra` raster layer clipped to the extent of the polygon.
#'
#' @details
#' The function first transforms the polygon to match the CRS of the raster, then crops the raster to the extent of the polygon.
#' Finally, it applies a mask to retain only the areas within the polygon.
#'
#' @examples
#' library(terra)
#' library(sf)
#' r <- rast(system.file("ex/logo.tif", package="terra"))
#' p <- st_as_sf(st_sfc(st_polygon(list(rbind(c(5, 5), c(5, 60), c(60, 60), c(60, 5), c(5, 5)))), crs = st_crs(r)))
#' result <- clip_raster(r, p)
#' plot(result)
#'
#' @export
clip_raster <- function(raster, polygon) {
  if (!inherits(raster, "SpatRaster")) {
    stop("The input 'raster' must be a 'terra' SpatRaster object.")
  }
  if (!inherits(polygon, "sf")) {
    stop("The input 'polygon' must be an 'sf' polygon object.")
  }

  tryCatch({
    s <- sf::st_transform(polygon, terra::crs(raster))
    r <- terra::crop(raster, s)
    terra::mask(r, s)
  }, error = function(e) {
    stop(sprintf("Error during raster clipping: %s", e$message))
  })
}
