test_that("aggregate_rasters processes raster files correctly", {
  temp_dir <- tempdir()

  input_dir <- system.file("extdata", "mdt", package = "sigugr")

  expect_true(dir.exists(input_dir))
  raster_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE,
                             ignore.case = TRUE)
  expect_true(length(raster_files) > 0)

  factor <- 2
  result_files <- aggregate_rasters(input_dir, temp_dir, factor = factor)

  expect_type(result_files, "character")
  expect_true(length(result_files) > 0)

  output_files <- list.files(temp_dir, pattern = "\\.TIF$", full.names = TRUE)
  expect_true(length(output_files) > 0)
  expect_equal(length(output_files), length(raster_files))

  for (file in output_files) {
    r <- terra::rast(file)
    expect_equal(terra::res(r), terra::res(terra::rast(raster_files[1])) * factor)
  }
})

test_that("aggregate_rasters creates the output directory if it does not exist", {
  input_dir <- file.path(tempdir(), "test_rasters_input")
  dir.create(input_dir, showWarnings = FALSE)

  r1 <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  input_file <- file.path(input_dir, "raster1.tif")
  terra::writeRaster(r1, input_file, overwrite = TRUE)

  # Output directory (must not exist at startup)
  output_dir <- file.path(tempdir(), "test_rasters_output")
  expect_false(dir.exists(output_dir)) # Confirmar que no existe

  aggregate_rasters(input_dir, output_dir, factor = 2)

  expect_true(dir.exists(output_dir))

  output_file <- file.path(output_dir, "raster1.TIF")
  expect_true(file.exists(output_file))

  r_aggregated <- terra::rast(output_file)
  expect_equal(ncol(r_aggregated), 5) # Original (10) dividido por factor (2)
  expect_equal(nrow(r_aggregated), 5)

  # Limpieza
  unlink(input_dir, recursive = TRUE)
  unlink(output_dir, recursive = TRUE)
})

