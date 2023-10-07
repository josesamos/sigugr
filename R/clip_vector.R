#' Clip a vector layer based on a polygon
#'
#' We must ensure that the layer to be cut has the correct encoding.
#'
#' Project the result to the CRS of the clipping polygon.
#'
#' @param vector A `sf` vector layer.
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
clip_vector <- function(vector, polygon) {
  s <- sf::st_transform(polygon, sf::st_crs(vector))
  r <- sf::st_intersection(vector, s)
  res <- sf::st_transform(r, sf::st_crs(polygon))
  res[names(vector)]
}
