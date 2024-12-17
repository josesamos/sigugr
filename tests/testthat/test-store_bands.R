test_that("store_bands stores all bands as separate tables in PostGIS", {
  # Temporary raster file with 3 simulated bands
  temp_raster <- tempfile(fileext = ".tif")
  sr <- terra::rast(nrows = 10, ncols = 10, nlyrs = 3, vals = runif(300))
  terra::writeRaster(sr, temp_raster, filetype = "GTiff", overwrite = TRUE)

  # Mock for rpostgis::pgWriteRast
  mock_pgWriteRast <- mockery::mock(NULL, cycle = TRUE)
  mock_conn <- mockery::mock("DB Connection")

  # Stub for rpostgis::pgWriteRast
  mockery::stub(store_bands, "rpostgis::pgWriteRast", mock_pgWriteRast)
  mockery::stub(store_bands, "name_raster_bands", setNames(seq_len(3), names(sr)))

  # Execute the function
  result <- store_bands(temp_raster, conn = mock_conn, schema = "test_schema")

  # Expected table names
  expected_tables <- snakecase::to_snake_case(names(sr))

  # Verifications
  expect_equal(result, expected_tables)
  mockery::expect_called(mock_pgWriteRast, 3)  # Called once per band
})


test_that("store_bands stops if the raster file does not exist", {
  # Define a non-existent file
  non_existent_raster <- "fake/path/to/nonexistent.tif"

  # Mock for the connection
  mock_conn <- mockery::mock("DB Connection")

  # Verify that an error is thrown if the file does not exist
  expect_error(
    store_bands(non_existent_raster, conn = mock_conn),
    "The specified raster file does not exist"
  )
})


test_that("store_bands applies prefix and postfix to table names", {
  # Temporary raster file
  temp_raster <- tempfile(fileext = ".tif")
  sr <- terra::rast(nrows = 10, ncols = 10, nlyrs = 2, vals = runif(200))
  names(sr) <- c("band1", "band2") # Assign explicit names to the bands
  terra::writeRaster(sr, temp_raster, filetype = "GTiff", overwrite = TRUE)

  # Mock for rpostgis::pgWriteRast
  mock_pgWriteRast <- mockery::mock(NULL, cycle = TRUE)
  mock_conn <- mockery::mock("DB Connection")

  # Stub for functions
  mockery::stub(store_bands, "rpostgis::pgWriteRast", mock_pgWriteRast)
  mockery::stub(
    store_bands,
    "name_raster_bands",
    setNames(c(1, 2), c("example_band1_raster", "example_band2_raster"))
  )

  # Execute with prefix and postfix
  result <- store_bands(
    temp_raster,
    conn = mock_conn,
    schema = "test_schema",
    prefix = "example_",
    postfix = "_raster"
  )

  # Expected table names
  expected_tables <- c("example_band_1_raster", "example_band_2_raster")

  # Verifications
  expect_equal(result, expected_tables)
  mockery::expect_called(mock_pgWriteRast, 2)
})
