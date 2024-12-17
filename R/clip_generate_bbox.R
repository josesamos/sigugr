#' Generate a Bounding Box as an sf Object
#'
#' Takes an `sf` object or a `terra::SpatRaster` as input and returns a new `sf` object
#' representing the bounding box (minimum bounding rectangle) of the input layer.
#'
#' @param layer An `sf` object or a `terra::SpatRaster` object.
#'
#' @return An `sf` object representing the bounding box of the input layer.
#'
#' @family clip functions
#'
#' @examples
#' # Example with a vector layer
#' source_gpkg <- system.file("extdata/sigugr.gpkg", package = "sigugr")
#' lanjaron <- sf::st_read(source_gpkg, layer = "lanjaron", quiet = TRUE)
#' bbox_vector <- generate_bbox(lanjaron)
#'
#' # Example with a raster layer
#' raster_file <- system.file("extdata/sat.tif", package = "sigugr")
#' raster <- terra::rast(raster_file)
#' bbox_raster <- generate_bbox(raster)
#'
#' @export
generate_bbox <- function(layer) {
  if (inherits(layer, "sf")) {
    bbox <- sf::st_bbox(layer)
    bbox_polygon <- sf::st_as_sfc(bbox)
    return(sf::st_sf(geometry = bbox_polygon, crs = sf::st_crs(layer)))
  } else if (inherits(layer, "SpatRaster")) {
    bbox <- terra::ext(layer)
    bbox_polygon <- terra::as.polygons(bbox)
    sf_bbox <- sf::st_as_sf(bbox_polygon)
    sf::st_crs(sf_bbox) <- terra::crs(layer)
    return(sf_bbox)
  } else {
    stop("Input layer must be an sf object or a terra::SpatRaster.")
  }
}
