test_that("pg_write_gpkg_layers modifies field names to Snake Case if specified", {
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
  mockery::stub(pg_write_gpkg_layers, "sf::st_write", mock_conn)

  # Ejecutamos la función de prueba
  result <- pg_write_gpkg_layers(
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

test_that("pg_write_gpkg_layers leaves field names unchanged if snake_case_fields is FALSE", {
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
  mockery::stub(pg_write_gpkg_layers, "sf::st_write", mock_conn)

  result <- pg_write_gpkg_layers(
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
