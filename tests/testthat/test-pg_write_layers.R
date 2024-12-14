test_that("pg_write_layers modifies field names to Snake Case if specified", {
  temp_gpkg <- tempfile(fileext = ".gpkg")
  layer <- sf::st_sf(
    geom = sf::st_sfc(sf::st_point(c(0, 0))),
    FieldName1 = 1:3,
    AnotherFieldName = c("a", "b", "c")
  )
  sf::st_write(layer, temp_gpkg, layer = "test_layer", quiet = TRUE)

  # Mock connection
  mock_conn <- mockery::mock(TRUE)

  # Stub sf::st_write to intercept calls
  mockery::stub(pg_write_layers, "sf::st_write", mock_conn)

  # Ejecutamos la función de prueba
  result <- pg_write_layers(
    gpkg = temp_gpkg,
    conn = "mock_connection",
    schema = "test_schema",
    prefix = "pre_",
    postfix = "_post",
    geom_colum = "geometry",
    snake_case_fields = TRUE
  )

  expect_equal(result, "pre_test_layer_post")

  # Aseguramos que la llamada a sf::st_write se realizó correctamente
  mockery::expect_called(mock_conn, 1)


  unlink(temp_gpkg)
})

test_that("pg_write_layers leaves field names unchanged if snake_case_fields is FALSE", {
  temp_gpkg <- tempfile(fileext = ".gpkg")
  layer <- sf::st_sf(
    geom = sf::st_sfc(sf::st_point(c(0, 0))),
    FieldName1 = 1:3,
    AnotherFieldName = c("a", "b", "c")
  )
  sf::st_write(layer, temp_gpkg, layer = "test_layer", quiet = TRUE)

  # Mock connection
  mock_conn <- mockery::mock(TRUE)

  # Stub sf::st_write to intercept calls
  mockery::stub(pg_write_layers, "sf::st_write", mock_conn)

  result <- pg_write_layers(
    gpkg = temp_gpkg,
    conn = "mock_connection",
    schema = "test_schema",
    prefix = "pre_",
    postfix = "_post",
    geom_colum = "geometry",
    snake_case_fields = FALSE
  )

  expect_equal(result, "pre_test_layer_post")

  # We ensure that the call to sf::st_write was successful
  mockery::expect_called(mock_conn, 1)

  unlink(temp_gpkg)
})


test_that("pg_write_layers throws an error if the GeoPackage file does not exist", {
  # Define a non-existent GeoPackage file path
  non_existent_gpkg <- tempfile(fileext = ".gpkg")

  # Mock connection
  mock_conn <- mockery::mock(TRUE)

  # Stub sf::st_write to intercept calls
  mockery::stub(pg_write_layers, "sf::st_write", mock_conn)

  # Expect error when trying to run the function with a non-existent GeoPackage
  expect_error(pg_write_layers(
    gpkg = non_existent_gpkg,
    conn = mock_conn,
    schema = "test_schema",
    prefix = "pre_",
    postfix = "_post",
    geom_colum = "geometry",
    snake_case_fields = TRUE
  ), "The GeoPackage file does not exist.")
})

test_that("pg_write_layers throws an error if there are no valid geometries in the GeoPackage", {
  # Create a temporary GeoPackage with a non-geographical layer
  temp_gpkg <- tempfile(fileext = ".gpkg")
  non_geometric_layer <- data.frame(Field1 = 1:3, Field2 = c("A", "B", "C"))
  sf::st_write(non_geometric_layer, temp_gpkg, layer = "non_geo_layer", quiet = TRUE)

  # Mock connection
  mock_conn <- mockery::mock(TRUE)

  # Stub sf::st_write to intercept calls
  mockery::stub(pg_write_layers, "sf::st_write", mock_conn)

  # Expect error when trying to run the function with no valid geometries
  expect_error(pg_write_layers(
    gpkg = temp_gpkg,
    conn = mock_conn,
    schema = "test_schema",
    prefix = "pre_",
    postfix = "_post",
    geom_colum = "geometry",
    snake_case_fields = TRUE
  ), "No layers with valid geometries found in the GeoPackage.")

  unlink(temp_gpkg)
})
