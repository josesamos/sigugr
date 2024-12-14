test_that("generate_bounding_box creates a valid bounding box layer", {
  source_gpkg <- system.file("extdata/sigugr.gpkg", package = "sigugr")
  lanjaron <- sf::st_read(source_gpkg, layer = "lanjaron", quiet = TRUE)

  bbox_layer <- generate_bbox(lanjaron)

  expect_s3_class(bbox_layer, "sf")

  expect_equal(as.character(unique(sf::st_geometry_type(bbox_layer))), "POLYGON")

  expect_equal(sf::st_crs(bbox_layer), sf::st_crs(lanjaron))

  original_bbox <- sf::st_bbox(lanjaron)
  generated_bbox <- sf::st_bbox(bbox_layer)
  expect_equal(original_bbox, generated_bbox)
})
