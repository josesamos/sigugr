#' Generate a Bounding Box as an sf Object
#'
#' This function takes an `sf` object as input and returns a new `sf` object
#' representing the bounding box (minimum bounding rectangle) of the input layer.
#'
#' @param layer An `sf` object, representing the input vector layer.
#'
#' @return An `sf` object representing the bounding box of the input layer.
#'
#' @examples
#' source_gpkg <- system.file("extdata/sigugr.gpkg", package = "sigugr")
#' lanjaron <- sf::st_read(source_gpkg, layer = "lanjaron", quiet = TRUE)
#'
#' bbox_layer <- generate_bbox(lanjaron)
#'
#' @export
generate_bbox <- function(layer) {
  if (!inherits(layer, "sf")) {
    stop("Input layer must be an sf object.")
  }

  bbox <- sf::st_bbox(layer)
  bbox_polygon <- sf::st_as_sfc(bbox)
  sf::st_sf(geometry = bbox_polygon, crs = sf::st_crs(layer))
}
