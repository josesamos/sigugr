source_gpkg <- system.file("extdata", "sigugr.gpkg", package = "sigugr")
p <-sf::st_read(source_gpkg, layer = 'lanjaron', quiet = TRUE)

source_tif <- system.file("extdata", "sat.tif", package = "sigugr")
r <- terra::rast(source_tif)


test_that("clip_raster returns a SpatRaster with correct dimensions", {
  result <- clip_raster(r, p)
  expect_s4_class(result, "SpatRaster")
  expect_true(ncol(result) > 0 && nrow(result) > 0)
})


test_that("clip_raster handles invalid raster input", {
  expect_error(clip_raster(data.frame(x = 1:5), p),
               "The input 'raster' must be a 'terra' SpatRaster object.")
})


test_that("clip_raster handles invalid polygon input", {
  expect_error(clip_raster(r, data.frame(x = 1:5)),
               "The input 'polygon' must be an 'sf' polygon object.")
})
