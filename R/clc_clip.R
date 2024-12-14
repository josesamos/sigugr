#' Clip a Vector Layer with a Polygon
#'
#' This function clips a vector layer (e.g., points, lines, polygons) using a polygon layer.
#' It handles CRS (Coordinate Reference System) transformations automatically if necessary,
#' ensuring the output is in the same CRS as the input polygon.
#'
#' @param vector An `sf` object representing the vector layer to be clipped.
#' @param polygon An `sf` object representing the polygon layer used for clipping.
#'
#' @return An `sf` object containing the features of the input `vector` that intersect with the `polygon`.
#' The output will be in the CRS of the `polygon`, and it will retain all attributes of the input `vector`.
#'
#' @examples
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#'
#' clc <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
#' lanjaron <- sf::st_read(gpkg_path, layer = "lanjaron", quiet = TRUE)
#'
#' clc_clipped <- clip_vector(clc, lanjaron)
#'
#' @export
clip_vector <- function(vector, polygon) {
  crs_polygon <- sf::st_crs(polygon)
  if (sf::st_crs(vector) != crs_polygon) {
    polygon <- sf::st_transform(polygon, sf::st_crs(vector))
  }
  # avoid warning attribute variables are assumed to be spatially constant...
  sf::st_agr(vector) = "constant"
  sf::st_agr(polygon) = "constant"
  # closed: the edges of the polygon are considered as part of the polygon
  res <- sf::st_intersection(vector, polygon, model = "closed")
  res <- res[names(vector)]
  if (sf::st_crs(vector) != crs_polygon) {
    res <- sf::st_transform(res, crs_polygon)
  }
  res
}


#' Safe Clip a Multipolygon Vector Layer
#'
#' This function clips a `MULTIPOLYGON` vector layer using a polygon layer, handling specific
#' issues that might arise with geometries encoded incorrectly or containing unknown WKB types.
#' It serves as a fallback when the `clip_vector()` function fails due to errors like
#' `ParseException: Unknown WKB type 12`, which is associated with *MULTIPOLYGON* types.
#'
#' The function ensures that the input layer is correctly encoded as `MULTIPOLYGON` and
#' uses GDAL utilities for re-encoding if necessary. The output is projected to the CRS
#' of the clipping polygon.
#'
#' This solution is inspired by a discussion on handling WKB type errors in R:
#' <https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12>.
#'
#' @param vector A `sf` multipolygon vector layer to be clipped.
#' @param polygon A `sf` polygon layer used as the clipping geometry.
#'
#' @return A `sf` vector layer with the clipped geometries.
#'
#' @examples
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#'
#' clc <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
#' lanjaron <- sf::st_read(gpkg_path, layer = "lanjaron", quiet = TRUE)
#'
#' clc_clipped <- clip_multipoligon(clc, lanjaron)
#'
#' @export
clip_multipoligon <- function(vector, polygon) {
  tryCatch({
    v <- sf::st_cast(vector, "MULTIPOLYGON")
    clip_vector(v, polygon)
  }, error = function(e) {
    f <- tempfile(fileext = ".gpkg")
    sf::st_write(vector, f, quiet = TRUE)
    g <- tempfile(fileext = ".gpkg")
    gdalUtilities::ogr2ogr(f, g, f = "GPKG", nlt = "MULTIPOLYGON")
    v <- sf::st_read(g, quiet = TRUE)
    clip_vector(v, polygon)
  })
}

