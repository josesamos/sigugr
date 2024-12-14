test_that("generate_bbox handles sf objects correctly", {
  source_gpkg <- system.file("extdata/sigugr.gpkg", package = "sigugr")
  lanjaron <- sf::st_read(source_gpkg, layer = "lanjaron", quiet = TRUE)

  bbox_sf <- generate_bbox(lanjaron)

  expect_s3_class(bbox_sf, "sf")
  expect_equal(as.character(unique(sf::st_geometry_type(bbox_sf))), "POLYGON")
  expect_equal(sf::st_crs(bbox_sf), sf::st_crs(lanjaron))
})

test_that("generate_bbox handles SpatRaster objects correctly", {
  raster_file <- system.file("extdata/sat.tif", package = "sigugr")
  raster <- terra::rast(raster_file)

  bbox_raster <- generate_bbox(raster)

  expect_s3_class(bbox_raster, "sf")
  expect_equal(as.character(unique(sf::st_geometry_type(bbox_raster))), "POLYGON")
  expect_equal(sf::st_crs(bbox_raster)$wkt, terra::crs(raster))
})

test_that("generate_bbox raises an error for unsupported types", {
  unsupported_input <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    generate_bbox(unsupported_input),
    "Input layer must be an sf object or a terra::SpatRaster."
  )
})

test_that("generate_bbox returns a valid bounding box", {
  source_gpkg <- system.file("extdata/sigugr.gpkg", package = "sigugr")
  lanjaron <- sf::st_read(source_gpkg, layer = "lanjaron", quiet = TRUE)

  bbox_sf <- generate_bbox(lanjaron)

  expected_bbox <- sf::st_as_sfc(sf::st_bbox(lanjaron))

  expect_true(sf::st_equals(bbox_sf$geometry, expected_bbox)[[1]] == 1)
})
