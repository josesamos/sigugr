#' Clip a Vector Layer with a Polygon
#'
#' Clips a vector layer using a polygon layer. It handles CRS transformations automatically
#' if necessary, ensuring the output is in the same CRS as the input polygon.
#'
#' @param vector An `sf` object representing the vector layer to be clipped.
#' @param polygon An `sf` object representing the polygon layer used for clipping.
#'
#' @return An `sf` object containing the features of the input `vector` that intersect with the `polygon`.
#' The output will be in the CRS of the `polygon`, and it will retain all attributes of the input `vector`.
#'
#' @family clip functions
#'
#' @examples
#' gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
#'
#' clc <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
#' lanjaron <- sf::st_read(gpkg_path, layer = "lanjaron", quiet = TRUE)
#'
#' clc_clipped <- clip_layer(clc, lanjaron)
#'
#' @export
clip_layer <- function(vector, polygon) {
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
  # Verify and transform geometries based on their type
  if (all(sf::st_geometry_type(res) %in% c("POLYGON", "MULTIPOLYGON"))) {
    res <- sf::st_cast(res, "MULTIPOLYGON")
  } else if (all(sf::st_geometry_type(res) %in% c("LINESTRING", "MULTILINESTRING"))) {
    res <- sf::st_cast(res, "MULTILINESTRING")
  } else if (all(sf::st_geometry_type(res) %in% c("POINT", "MULTIPOINT"))) {
    res <- sf::st_cast(res, "MULTIPOINT")
  } else if (all(
    sf::st_geometry_type(res) %in% c("LINESTRING", "MULTILINESTRING", "POINT", "MULTIPOINT")
  )) {
    # Keep only geometries that are LINESTRING or MULTILINESTRING
    res <- res[sf::st_geometry_type(res) %in% c("LINESTRING", "MULTILINESTRING"), ]
    res <- sf::st_cast(res, "MULTILINESTRING")
  }  else if (all(
    sf::st_geometry_type(res) %in% c(
      "POLYGON",
      "MULTIPOLYGON",
      "LINESTRING",
      "MULTILINESTRING",
      "POINT",
      "MULTIPOINT"
    )
  )) {
    # Keep only geometries that are POLYGON or "MULTIPOLYGON
    res <- res[sf::st_geometry_type(res) %in% c("POLYGON", "MULTIPOLYGON"), ]
    res <- sf::st_cast(res, "MULTIPOLYGON")
  } else {
    stop("Unsupported or mixed geometry types: Please check the layer content.")
  }
  res
}


#' Safe Clip a Multipolygon Vector Layer
#'
#' Clips a `MULTIPOLYGON` vector layer using a polygon layer, handling specific
#' issues that might arise with geometries encoded incorrectly or containing unknown WKB types.
#' It serves as a fallback when the `clip_layer` function fails due to errors like
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
#' @family clip functions
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
    clip_layer(v, polygon)
  }, error = function(e) {
    f <- tempfile(fileext = ".gpkg")
    sf::st_write(vector, f, quiet = TRUE)
    g <- tempfile(fileext = ".gpkg")
    gdalUtilities::ogr2ogr(f, g, f = "GPKG", nlt = "MULTIPOLYGON")
    v <- sf::st_read(g, quiet = TRUE)
    clip_layer(v, polygon)
  })
}

