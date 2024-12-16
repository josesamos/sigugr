
# Test for invalid SpatRaster input
test_that("store_raster throws an error if sr is not SpatRaster", {
  expect_error(
    store_raster("not_a_raster", conn, table_name = "invalid_table"),
    "`sr` must be a terra::SpatRaster object."
  )
})


test_that("store_raster works correctly with mock database connection", {
  # Mock database connection and functions
  conn <- mockery::mock()
  mock_pgWriteRast <- mockery::mock(NULL)

  # Patch rpostgis::pgWriteRast with the mock function
  mockery::stub(store_raster, "rpostgis::pgWriteRast", mock_pgWriteRast)

  # Create a sample SpatRaster object
  rast <- terra::rast(ncol = 10, nrow = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(rast) <- runif(terra::ncell(rast))

  # Define table name
  table_name <- "TestTable"

  # Test store_raster
  expect_no_error(store_raster(rast, conn, table_name = table_name))

  # Verify pgWriteRast was called with correct arguments
  mockery::expect_called(mock_pgWriteRast, 1)
})
