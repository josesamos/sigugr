test_that("copy_styles_layer copies styles correctly", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  temp_gpkg_no_styles <- tempfile(fileext = ".gpkg")
  sf::st_layers(source_gpkg)$name |>
    setdiff("layer_styles") |>
    purrr::walk(~{
      layer_data <- sf::st_read(source_gpkg, layer = .x, quiet = TRUE)
      sf::st_write(layer_data, temp_gpkg_no_styles, layer = .x, quiet = TRUE)
    })

  copy_styles_layer(from = source_gpkg, to = temp_gpkg_no_styles)

  # Verify the styles have been copied
  dest_styles <- sf::st_read(temp_gpkg_no_styles, layer = "layer_styles", quiet = TRUE)
  expect_gt(nrow(dest_styles), 0)
})


test_that("get_layer_categories extracts categories correctly from GeoPackage", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
  raster_path <- system.file("extdata", "clc.tif", package = "clc")
  r_clc <- terra::rast(raster_path)

  categories <- get_layer_categories(from = source_gpkg, r_clc = r_clc)

  expect_s3_class(categories, "data.frame")

  expect_true(all(c("id", "description", "color") %in% colnames(categories)))

  # Verify that the IDs match raster values
  raster_values <- unique(terra::values(r_clc))
  expect_true(all(categories$id %in% raster_values))
})


test_that("copy_styles_layer_names works with PostGIS database connection", {
  # Mock inputs
  mock_from <- "mock_source" # Simulated source, e.g., GeoPackage or DB connection
  mock_to <- "mock_destination" # Simulated destination PostGIS connection
  mock_layers <- c("layer1", "layer2") # Mock layer names
  mock_database <- "test_db"
  mock_schema <- "public"
  mock_style <- data.frame(style_name = "mock_style", value = "mock_value") # Mock style

  # Mocks for dependent functions
  mock_read_style_from_source <- mockery::mock(mock_style)
  mock_assign_styles_to_layers <- mockery::mock(invisible(mock_style))

  # Stubbing internal functions
  mockery::stub(copy_styles_layer_names, "read_style_from_source", mock_read_style_from_source)
  mockery::stub(copy_styles_layer_names, "assign_styles_to_layers", mock_assign_styles_to_layers)

  # Call the function
  result <- copy_styles_layer_names(
    from = mock_from,
    to = mock_to,
    layers = mock_layers,
    database = mock_database,
    schema = mock_schema
  )

  # Validate calls and arguments for `read_style_from_source`
  mockery::expect_called(mock_read_style_from_source, 1)
  mockery::expect_args(mock_read_style_from_source, 1, mock_from)

  # Validate calls and arguments for `assign_styles_to_layers`
  mockery::expect_called(mock_assign_styles_to_layers, 1)
  mockery::expect_args(
    mock_assign_styles_to_layers,
    1,
    style = mock_style,
    to = mock_to,
    database = mock_database,
    schema = mock_schema,
    layers = mock_layers
  )

  # Validate final result
  expect_equal(result, invisible(mock_style))
})
