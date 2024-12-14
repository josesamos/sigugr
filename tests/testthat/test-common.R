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
