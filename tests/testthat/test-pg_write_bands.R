test_that("pg_write_bands writes raster bands to PostGIS correctly", {
  # Mock connection and raster input
  conn <- mockery::mock()
  sr <- terra::rast(nrows = 10, ncols = 10, nlyrs = 3, vals = runif(300))
  names(sr) <- c("band1", "band2", "band3")

  # Mock the pgWriteRast function
  mock_pgWriteRast <- mockery::mock()
  mockery::stub(pg_write_bands, "rpostgis::pgWriteRast", mock_pgWriteRast)

  # Call the function
  result <- pg_write_bands(sr, conn, schema = "test_schema", prefix = "pre_", postfix = "_post")

  # Check that pgWriteRast was called the correct number of times
  expect_equal(length(mockery::mock_args(mock_pgWriteRast)), 3)

  # Capture arguments passed to pgWriteRast
  calls <- mockery::mock_args(mock_pgWriteRast)

  # Verify each call
  expect_equal(calls[[1]][[1]], conn)
  expect_equal(calls[[1]][[2]], c("test_schema", "pre_band_1_post"))
  expect_s4_class(calls[[1]][[3]], "SpatRaster")
  expect_equal(calls[[2]][[2]], c("test_schema", "pre_band_2_post"))
  expect_equal(calls[[3]][[2]], c("test_schema", "pre_band_3_post"))

  # Check the return value
  expect_equal(result, c("pre_band_1_post", "pre_band_2_post", "pre_band_3_post"))
})

test_that("pg_write_bands handles invalid inputs", {
  # Invalid raster
  expect_error(
    pg_write_bands(NULL, mockery::mock()),
    "`sr` must be a terra::SpatRaster object."
  )

  # Missing band names
  sr <- terra::rast(nrows = 10, ncols = 10, nlyrs = 3, vals = runif(300))
  names(sr) <- NULL
  expect_error(
    pg_write_bands(sr, mockery::mock()),
    "The SpatRaster object `sr` must have bands with different names."
  )
})
