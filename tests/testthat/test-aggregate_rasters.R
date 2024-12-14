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

