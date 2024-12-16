#' Create a Safe Buffer Around a Spatial Layer
#'
#' This function generates a buffer around all geometries in a spatial layer.
#' If the layer's coordinate reference system (CRS) is not projected, it will
#' be reprojected to an appropriate projected CRS before calculating the buffer.
#' After the buffer is applied, the geometries are optionally reprojected back
#' to the original CRS.
#'
#' @param layer An `sf` object representing the spatial layer to be buffered.
#' @param distance A numeric value specifying the buffer distance. The distance
#'   is expressed in meters.
#'
#' @return An `sf` object with the buffered geometries.
#'
#' @examples
#' layer <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(
#'   rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))
#' ))), crs = 4326)
#'
#' # Generate a buffer with a 500-meter distance
#' buffered_layer <- safe_buffer (layer, distance = 500)
#'
#' @keywords internal
#' @noRd
safe_buffer  <- function(layer, distance) {
  crs <- sf::st_crs(layer)$epsg

  pr_crs <- get_projected_crs(layer)

  if (crs != pr_crs) {
    # Reproject the layer to the projected CRS
    layer <- sf::st_transform(layer, crs = pr_crs)
  }

  # Compute the buffer using the specified distance
  layer <- sf::st_buffer(layer, dist = distance)

  if (crs != pr_crs) {
    layer <- sf::st_transform(layer, crs = crs)
  }

  return(layer)
}
