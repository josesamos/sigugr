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



test_that("clip_raster handles valid inputs correctly", {
  # Create a test raster and polygon
  test_raster <- terra::rast(nrows = 10, ncols = 10,
                             xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                             crs = "EPSG:4326")
  terra::values(test_raster) <- 1:100

  test_polygon <- sf::st_sfc(sf::st_polygon(list(
    matrix(c(2, 2, 8, 8, 2, 2, 2, 8, 8, 2), ncol = 2)
  )), crs = 4326)
  test_polygon_sf <- sf::st_sf(geometry = test_polygon)

  # Test the clipping
  clipped <- clip_raster(test_raster, test_polygon_sf)
  expect_s4_class(clipped, "SpatRaster")
  expect_equal(terra::ncell(clipped), 36)
})

test_that("clip_raster projects raster if CRS differs", {
  # Create raster and polygon with different CRS
  test_raster <- terra::rast(nrows = 10, ncols = 10,
                             xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                             crs = "EPSG:32633")
  terra::values(test_raster) <- 1:100

  test_polygon <- sf::st_sfc(sf::st_polygon(list(
    matrix(c(2, 2, 8, 8, 2, 2, 2, 8, 8, 2), ncol = 2)
  )), crs = 32633)
  test_polygon_sf <- sf::st_sf(geometry = test_polygon)
  test_polygon_sf <- sf::st_transform(test_polygon_sf, crs = 4326)

  # Test projection and clipping
  clipped <- clip_raster(test_raster, test_polygon_sf, keep_crs = FALSE)
  expect_s4_class(clipped, "SpatRaster")
})
