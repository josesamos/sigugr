
test_that("read_style_from_source works correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  expect_true(file.exists(gpkg_path))

  style <- read_style_from_source(gpkg_path)
  expect_s3_class(style, "data.frame")
  expect_true("f_table_name" %in% colnames(style))

  layer_name <- "clc"
  style_specific <- read_style_from_source(gpkg_path, layer_name = layer_name)
  expect_s3_class(style_specific, "data.frame")
  expect_true(all(style_specific$f_table_name == layer_name))

  nonexistent_layer <- "no_such_layer"
  expect_error(
    read_style_from_source(gpkg_path, layer_name = nonexistent_layer),
    regexp = "No style found for the specified layer name"
  )
})


test_that("assign_styles_to_layers works correctly", {
  original_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  expect_true(file.exists(original_gpkg))

  temp_gpkg_no_styles <- tempfile(fileext = ".gpkg")
  sf::st_layers(original_gpkg)$name |>
    setdiff("layer_styles") |>
    purrr::walk(~{
      layer_data <- sf::st_read(original_gpkg, layer = .x, quiet = TRUE)
      sf::st_write(layer_data, temp_gpkg_no_styles, layer = .x, quiet = TRUE)
    })

  temp_gpkg_with_styles <- tempfile(fileext = ".gpkg")
  sf::st_layers(original_gpkg)$name |>
    purrr::walk(~{
      layer_data <- sf::st_read(original_gpkg, layer = .x, quiet = TRUE)
      sf::st_write(layer_data, temp_gpkg_with_styles, layer = .x, quiet = TRUE)
    })

  # Case 1: Assign styles when there are no styles in the destination
  style <- read_style_from_source(original_gpkg, layer_name = "clc")
  assign_styles_to_layers(
    style,
    to = temp_gpkg_no_styles,
    layers = "clc"
  )

  styles_no_styles <- sf::st_read(temp_gpkg_no_styles, layer = "layer_styles", quiet = TRUE)
  expect_s3_class(styles_no_styles, "data.frame")
  expect_true("clc" %in% styles_no_styles$f_table_name)

  # Case 2: Update styles when they already exist in the destination
  style <- read_style_from_source(original_gpkg, layer_name = "clc")
  assign_styles_to_layers(
    style,
    to = temp_gpkg_with_styles,
    layers = c("lanjaron")
  )

  # Verify that the styles for the selected layers have been updated
  styles_with_styles <- sf::st_read(temp_gpkg_with_styles, layer = "layer_styles", quiet = TRUE)
  updated_styles <- styles_with_styles[styles_with_styles$f_table_name %in% c("lanjaron"), ]
  expect_equal(nrow(updated_styles), 1)

  # Verify that unmodified layer styles remain intact
  unmodified_styles <- styles_with_styles[!(styles_with_styles$f_table_name %in% c("lanjaron")), ]
  expect_equal(nrow(unmodified_styles), 1)

  # Case 3: Assign styles to all layers when layers is not specified
  style <- read_style_from_source(original_gpkg)
  assign_styles_to_layers(
    style,
    to = temp_gpkg_no_styles
  )

  # Verify that all layers have styles assigned
  all_layers_styles <- sf::st_read(temp_gpkg_no_styles, layer = "layer_styles", quiet = TRUE)
  expect_equal(sort(unique(all_layers_styles$f_table_name)), sort(setdiff(sf::st_layers(temp_gpkg_no_styles)$name, "layer_styles")))
})



test_that("get_all_layers_pg returns layers correctly", {
  # Mock the database connection
  mock_conn <- "mock_connection"

  # Define the schema to test
  schema_to_test <- "public"

  # Mock the expected query result
  mock_query_result <- data.frame(
    schema_name = c("public", "public", "other"),
    table_name = c("layer1", "layer2", "layer3"),
    geometry_column = c("geom", "geom", "geom"),
    geometry_type = c("geometry", "geometry", "geography"),
    stringsAsFactors = FALSE
  )

  # Mock dbGetQuery to return the mock data
  mock_dbGetQuery <- mockery::mock(mock_query_result)
  mockery::stub(get_all_layers_pg, "RPostgres::dbGetQuery", mock_dbGetQuery)

  # Call the function with the "public" schema
  result <- get_all_layers_pg(mock_conn, schema_to_test)

  # Check that dbGetQuery was called exactly once
  mockery::expect_called(mock_dbGetQuery, 1)

  # Verify the arguments passed to dbGetQuery
  expected_query <- "
  SELECT
    table_schema AS schema_name,
    table_name,
    column_name AS geometry_column,
    udt_name AS geometry_type
  FROM information_schema.columns
  WHERE udt_name IN ('geometry', 'geography');
"
  mockery::expect_args(mock_dbGetQuery, 1, mock_conn, expected_query)

  # Validate the function's output
  expected_result <- c("layer1", "layer2")
  expect_equal(result, expected_result)
})


test_that("exist_layer_styles_pg correctly checks table existence", {
  # Mock the database connection
  mock_conn <- "mock_connection"

  # Define the schema to test
  schema_to_test <- "public"

  # Mock the table name
  table_name <- "layer_styles"

  # Simulate a case where the table exists
  mock_query_result_exists <- data.frame(V1 = 1, stringsAsFactors = FALSE)

  # Simulate a case where the table does not exist
  mock_query_result_not_exists <- data.frame()

  # Mock dbGetQuery for both scenarios
  mock_dbGetQuery <- mockery::mock(
    mock_query_result_exists,   # First call: table exists
    mock_query_result_not_exists # Second call: table does not exist
  )
  mockery::stub(exist_layer_styles_pg, "RPostgres::dbGetQuery", mock_dbGetQuery)

  # Case 1: Table exists
  result_exists <- exist_layer_styles_pg(mock_conn, schema_to_test)

  # Check that dbGetQuery was called exactly once so far
  mockery::expect_called(mock_dbGetQuery, 1)

  # Validate the query sent to dbGetQuery
  expected_query <- sprintf(
    "
    SELECT 1
    FROM information_schema.tables
    WHERE table_name = '%s' AND table_schema = '%s';",
    table_name,
    schema_to_test
  )
  mockery::expect_args(mock_dbGetQuery, 1, mock_conn, expected_query)

  # Validate the function's output
  expect_equal(result_exists, table_name)

  # Case 2: Table does not exist
  result_not_exists <- exist_layer_styles_pg(mock_conn, schema_to_test)

  # Verify dbGetQuery was called twice in total
  mockery::expect_called(mock_dbGetQuery, 2)

  # Validate the output when the table does not exist
  expect_null(result_not_exists)
})


test_that("assign_styles_to_layers works with PostGIS database connection", {
  # Mock inputs
  mock_conn <- "mock_connection" # Simulated PostGIS connection
  schema <- "public"
  database <- "test_db"
  layers_input <- c("layer1", "layer2") # Mock layers specified
  mock_style <- data.frame(style_name = "mock_style", value = "mock_value") # Mock style

  # Mock outputs for internal functions
  mock_all_layers <- c("layer1", "layer2", "layer3")
  mock_existing_styles <- data.frame(layer_name = c("layer1"), style_data = "existing_style")
  mock_new_styles <- data.frame(layer_name = c("layer2"), style_data = "new_style")
  mock_combined_styles <- data.frame(
    layer_name = c("layer1", "layer2"),
    style_data = c("existing_style", "new_style")
  )

  # Mocks for dependent functions
  mock_get_all_layers_pg <- mockery::mock(mock_all_layers)
  mock_exist_layer_styles_pg <- mockery::mock("layer_styles")
  mock_get_layers_to_copy <- mockery::mock(layers_input)
  mock_get_existing_styles <- mockery::mock(mock_existing_styles)
  mock_generate_new_styles <- mockery::mock(mock_new_styles)
  mock_combine_styles <- mockery::mock(mock_combined_styles)

  # Stubbing internal functions
  mockery::stub(assign_styles_to_layers, "get_all_layers_pg", mock_get_all_layers_pg)
  mockery::stub(assign_styles_to_layers, "exist_layer_styles_pg", mock_exist_layer_styles_pg)
  mockery::stub(assign_styles_to_layers, "get_layers_to_copy", mock_get_layers_to_copy)
  mockery::stub(assign_styles_to_layers, "get_existing_styles", mock_get_existing_styles)
  mockery::stub(assign_styles_to_layers, "generate_new_styles", mock_generate_new_styles)
  mockery::stub(assign_styles_to_layers, "combine_styles", mock_combine_styles)

  # Call the function
  result <- assign_styles_to_layers(
    style = mock_style,
    to = mock_conn,
    database = database,
    schema = schema,
    layers = layers_input
  )

  # Validate calls and arguments
  mockery::expect_called(mock_get_all_layers_pg, 1)
  mockery::expect_args(mock_get_all_layers_pg, 1, mock_conn, schema)

  mockery::expect_called(mock_exist_layer_styles_pg, 1)
  mockery::expect_args(mock_exist_layer_styles_pg, 1, mock_conn, schema)

  mockery::expect_called(mock_get_layers_to_copy, 1)
  mockery::expect_args(mock_get_layers_to_copy, 1, layers_input, mock_all_layers)

  mockery::expect_called(mock_get_existing_styles, 1)
  mockery::expect_args(mock_get_existing_styles, 1, mock_conn, "layer_styles", mock_style)

  mockery::expect_called(mock_generate_new_styles, 1)
  mockery::expect_args(mock_generate_new_styles, 1, layers_input, mock_style, database, schema)

  mockery::expect_called(mock_combine_styles, 1)
  mockery::expect_args(mock_combine_styles, 1, mock_existing_styles, mock_new_styles, layers_input, mock_conn)

  # Validate final result
  expect_equal(result, invisible(mock_combined_styles))
})


test_that("read_style_from_source reads styles from a GeoPackage", {
  # Create a temporary GeoPackage file with a style table
  temp_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(
    sf::st_sf(
      f_table_name = c("layer1", "layer2"),  # Layer names
      styleQML = c("qml1", "qml2"),         # Example QML styles
      styleSLD = c("sld1", "sld2"),         # Example SLD styles
      geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),  # Dummy geometry
      crs = 4326
    ),
    temp_gpkg,
    layer = "layer_styles",
    quiet = TRUE
  )

  # Test reading the full style table
  style <- read_style_from_source(temp_gpkg)
  expect_s3_class(style, "sf")  # Verify the result is an sf object
  expect_equal(nrow(style), 1)  # By default, only the first style is returned

  # Test reading a specific layer's style
  style_layer1 <- read_style_from_source(temp_gpkg, layer_name = "layer1")
  expect_equal(style_layer1$f_table_name, "layer1")
  expect_equal(style_layer1$styleQML, "qml1")

  # Test handling of a non-existent layer
  expect_error(
    read_style_from_source(temp_gpkg, layer_name = "nonexistent_layer"),
    "No style found for the specified layer name"
  )
})

test_that("read_style_from_source handles all PostGIS-style columns", {
  # Create data with these columns
  postgis_style <- sf::st_sf(
    id = 1:2,                                # Simulated PostGIS ID column
    f_table_catalog = c("catalog1", "catalog2"),
    f_table_schema = c("schema1", "schema2"),
    f_table_name = c("layer1", "layer2"),
    f_geometry_column = c("geom1", "geom2"),
    stylename = c("style1", "style2"),
    styleqml = c("qml1", "qml2"),
    stylesld = c("sld1", "sld2"),
    useasdefault = c(TRUE, FALSE),
    description = c("desc1", "desc2"),
    owner = c("owner1", "owner2"),
    ui = c("ui1", "ui2"),
    update_time = as.POSIXct(c("2024-01-01", "2024-01-02")),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),  # Dummy geometry
    crs = 4326
  )

  # Write the PostGIS-style data to a GeoPackage
  temp_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(postgis_style, temp_gpkg, layer = "layer_styles", quiet = TRUE)

  # Read the data and ensure transformation logic is applied
  style <- read_style_from_source(temp_gpkg)

  # Verify that all columns are present after transformation
  transformed_columns <- c(
    "f_table_catalog",
    "f_table_schema",
    "f_table_name",
    "f_geometry_column",
    "styleName",
    "styleQML",
    "styleSLD",
    "useAsDefault",
    "description",
    "owner",
    "ui",
    "update_time"
  )

  expect_true(all(transformed_columns %in% names(style)))
  expect_equal(style$f_table_catalog, "")  # Catalog should be empty after transformation
  expect_equal(style$f_table_schema, "")   # Schema should be empty after transformation
  expect_equal(style$f_table_name[1], "layer1")
  expect_equal(style$useAsDefault[1], TRUE)
})
