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

