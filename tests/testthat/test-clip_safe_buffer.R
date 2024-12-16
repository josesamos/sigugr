test_that("safe_buffer works correctly with valid polygons", {

  # Create a valid polygon
  valid_polygon <- sf::st_sfc(
    sf::st_polygon(list(
      matrix(
        c(
          0, 0,
          10, 0,
          10, 10,
          0, 10,
          0, 0
        ),
        ncol = 2, byrow = TRUE
      )
    )), crs = 4326
  )

  # Apply the buffer
  buffered_polygon <- safe_buffer(valid_polygon, distance = 1000)

  # Test if output is still valid
  expect_true(sf::st_is_valid(buffered_polygon))

  # Test if the area has increased
  expect_true(sf::st_area(buffered_polygon) > sf::st_area(valid_polygon))

  # Test if CRS is preserved
  expect_equal(sf::st_crs(buffered_polygon), sf::st_crs(valid_polygon))
})


# Additional Test: Projected CRS
test_that("safe_buffer works with projected CRS", {

  # Create a valid polygon
  valid_polygon <- sf::st_sfc(
    sf::st_polygon(list(
      matrix(
        c(
          0, 0,
          10, 0,
          10, 10,
          0, 10,
          0, 0
        ),
        ncol = 2, byrow = TRUE
      )
    )), crs = 4326
  )
  # Create a valid polygon with a projected CRS
  valid_polygon_proj <- sf::st_transform(valid_polygon, crs = 3857)

  # Apply the buffer
  buffered_polygon_proj <- safe_buffer(valid_polygon_proj, distance = 1000)

  # Test if output is still valid
  expect_true(sf::st_is_valid(buffered_polygon_proj))

  # Test if the area has increased
  expect_true(sf::st_area(buffered_polygon_proj) > sf::st_area(valid_polygon_proj))

  # Test if CRS is preserved
  expect_equal(sf::st_crs(buffered_polygon_proj), sf::st_crs(valid_polygon_proj))
})
