test_that("get_nexus handles paths correctly", {
  expect_equal(get_nexus("output/"), "")
  expect_equal(get_nexus("output"), "/")
  expect_equal(get_nexus("/path/to/folder/"), "")
  expect_equal(get_nexus("/path/to/folder"), "/")
})


test_that("list_dir_rasters works for directories with raster files", {
  # Setup: Create a temporary directory with some raster files
  temp_dir <- tempdir()
  tif_file <- file.path(temp_dir, "example1.tif")
  jp2_file <- file.path(temp_dir, "example2.jp2")
  other_file <- file.path(temp_dir, "example.txt")
  file.create(c(tif_file, jp2_file, other_file))

  # Function call
  result <- list_dir_rasters(temp_dir)

  # Expectations
  expect_type(result, "character")  # Result should be a character vector
  expect_true(tif_file %in% result)  # Check .tif file is listed
  expect_true(jp2_file %in% result)  # Check .jp2 file is listed
  expect_false(other_file %in% result)  # Check .txt file is not listed
})

test_that("list_dir_rasters returns empty vector for empty directories", {
  # Setup: Create a temporary empty directory
  empty_dir <- tempfile()
  dir.create(empty_dir)

  # Function call
  result <- list_dir_rasters(empty_dir)

  # Expectations
  expect_type(result, "character")  # Result should be a character vector
  expect_length(result, 0)  # Result should be empty
})


test_that("get_projected_crs returns correct UTM CRS for geographic coordinates", {
  # Create a sample sf object with geographic CRS (WGS84)
  sample_points <- sf::st_sfc(
    sf::st_point(c(-77.0369, 38.9072)), # Washington, DC
    sf::st_point(c(139.6917, 35.6895))  # Tokyo, Japan
  )
  sample_sf <- sf::st_sf(geometry = sample_points, crs = 4326)

  # Check UTM EPSG codes
  expect_equal(get_projected_crs(sample_sf[1, ]), 32618) # Washington, DC (UTM 18N)
  expect_equal(get_projected_crs(sample_sf[2, ]), 32654) # Tokyo, Japan (UTM 54N)
})

test_that("get_projected_crs handles Southern Hemisphere correctly", {
  # Create a point in the Southern Hemisphere
  point_south <- sf::st_sfc(sf::st_point(c(-58.3816, -34.6037)), crs = 4326) # Buenos Aires
  sample_sf_south <- sf::st_sf(geometry = point_south)

  # Check UTM EPSG code for Southern Hemisphere
  expect_equal(get_projected_crs(sample_sf_south), 32721) # Buenos Aires (UTM 21S)
})

test_that("get_projected_crs returns layer's CRS if already projected", {
  # Create a projected sf object
  projected_layer <- sf::st_sfc(sf::st_point(c(500000, 4649776)), crs = 32633) # UTM Zone 33N
  projected_sf <- sf::st_sf(geometry = projected_layer)

  # Expect the same CRS to be returned
  expect_equal(get_projected_crs(projected_sf), 32633)
})

test_that("name_raster_bands assigns names to all bands when no bands are specified", {
  # Create a SpatRaster with 3 bands
  sr <- terra::rast(ncols = 10, nrows = 10, nlyr = 3)
  names(sr) <- c("red", "green", "blue")

  # Call the function without specifying bands
  result <- name_raster_bands(sr, prefix = "BAND_", postfix = "_final")

  # Expected result
  expected <- c(1, 2, 3)
  names(expected) <- c("BAND_red_final", "BAND_green_final", "BAND_blue_final")

  expect_equal(result, expected)
})

test_that("name_raster_bands handles unnamed bands and generates default names", {
  # Create a SpatRaster with unnamed bands
  sr <- terra::rast(ncols = 10, nrows = 10, nlyr = 3)
  names(sr) <- NULL

  # Call the function
  result <- name_raster_bands(sr, prefix = "Layer_", postfix = "_processed")

  # Expected result
  expected <- c(1, 2, 3)
  names(expected) <- c("Layer_Band_1_processed", "Layer_Band_2_processed", "Layer_Band_3_processed")

  expect_equal(result, expected)
})

test_that("name_raster_bands processes only specified bands", {
  # Create a SpatRaster with 3 bands
  sr <- terra::rast(ncols = 10, nrows = 10, nlyr = 3)
  names(sr) <- c("red", "green", "blue")

  # Specify bands 1 and 3
  result <- name_raster_bands(sr, prefix = "B_", postfix = "_v1", bands = c(1, 3))

  # Expected result
  expected <- c(1, 3)
  names(expected) <- c("B_red_v1", "B_blue_v1")

  expect_equal(result, expected)
})

test_that("name_raster_bands throws an error for out-of-bounds band indices", {
  # Create a SpatRaster with 3 bands
  sr <- terra::rast(ncols = 10, nrows = 10, nlyr = 3)

  # Specify invalid band indices
  expect_error(name_raster_bands(sr, bands = c(1, 4)),
               "Some band indices are out of bounds.")
  expect_error(name_raster_bands(sr, bands = c(-1, 2)),
               "Some band indices are out of bounds.")
})

test_that("name_raster_bands throws an error for duplicate band names", {
  # Create a SpatRaster with duplicate band names
  sr <- terra::rast(ncols = 10, nrows = 10, nlyr = 3)
  names(sr) <- c("red", "red", "blue")

  # Expect error for duplicate band names
  expect_error(name_raster_bands(sr),
               "The raster must have bands with different names.")
})

test_that("name_raster_bands works with unnamed band indices input", {
  # Create a SpatRaster with 3 bands
  sr <- terra::rast(ncols = 10, nrows = 10, nlyr = 3)
  names(sr) <- c("B1", "B2", "B3")

  # Unnamed indices provided for specific bands
  result <- name_raster_bands(sr, prefix = "Layer_", bands = c(1, 2))

  # Expected result
  expected <- c(1, 2)
  names(expected) <- c("Layer_B1", "Layer_B2")

  expect_equal(result, expected)
})
