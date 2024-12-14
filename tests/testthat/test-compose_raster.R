test_that("compose_raster combines rasters into a VRT", {
  # Mock directories and files
  temp_dir1 <- tempfile("dir1_")
  temp_dir2 <- tempfile("dir2_")
  dir.create(temp_dir1)
  dir.create(temp_dir2)

  # Create mock raster files
  r1 <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  r2 <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  r3 <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  terra::writeRaster(r1, file.path(temp_dir1, "raster1.tif"), overwrite = TRUE)
  terra::writeRaster(r2, file.path(temp_dir1, "raster2.tif"), overwrite = TRUE)
  terra::writeRaster(r3, file.path(temp_dir2, "raster3.tif"), overwrite = TRUE)

  # Test combining rasters from one directory
  combined_raster <- compose_raster(temp_dir1)
  expect_s4_class(combined_raster, "SpatRaster")

  # Test combining rasters from multiple directories
  combined_raster <- compose_raster(c(temp_dir1, temp_dir2))
  expect_s4_class(combined_raster, "SpatRaster")

  # Test custom output file
  output_file <- tempfile("custom_output_")
  combined_raster <- compose_raster(c(temp_dir1, temp_dir2), out_file = output_file)
  expect_s4_class(combined_raster, "SpatRaster")
  expect_true(file.exists(paste0(output_file, ".vrt")))

  # Clean up temporary files and directories
  unlink(temp_dir1, recursive = TRUE)
  unlink(temp_dir2, recursive = TRUE)
})
