test_that("publish_layer_set.geoserver publishes all layers successfully", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  # Simular el resultado de st_read
  query_result <- data.frame(f_table_name = c("layer1", "layer2", "layer3"))
  mock_sf_read <- mockery::mock(query_result)

  # Interceptar st_read
  mockery::stub(publish_layer_set.geoserver, "sf::st_read", mock_sf_read)

  # Simular publicación
  mock_publish_layer <- mockery::mock(0, cycle = TRUE)
  mockery::stub(publish_layer_set.geoserver, "publish_layer", mock_publish_layer)

  result <- publish_layer_set.geoserver(gso, source = NULL)
  expect_equal(result, 0)
  mockery::expect_called(mock_sf_read, 1)
  mockery::expect_called(mock_publish_layer, 3)
})

test_that("publish_layer_set.geoserver handles layer publishing error", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  # Simular resultado de st_read
  query_result <- data.frame(f_table_name = c("layer1", "layer2", "layer3"))
  mock_sf_read <- mockery::mock(query_result)
  mockery::stub(publish_layer_set.geoserver, "sf::st_read", mock_sf_read)

  # Simular error de publicación
  mock_publish_layer <- mockery::mock(0, 1, cycle = TRUE)
  mockery::stub(publish_layer_set.geoserver, "publish_layer", mock_publish_layer)

  expect_message(
    result <- publish_layer_set.geoserver(gso, source = NULL),
    regexp = "Not all available layers have been published\\."
  )
  expect_equal(result, 1)
  mockery::expect_called(mock_sf_read, 1)
  mockery::expect_called(mock_publish_layer, 2)
})

test_that("publish_layer_set.geoserver uses specified layers", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  # Capas específicas
  layers <- c("custom_layer1", "custom_layer2")

  # Simular publicación
  mock_publish_layer <- mockery::mock(0, cycle = TRUE)
  mockery::stub(publish_layer_set.geoserver, "publish_layer", mock_publish_layer)

  result <- publish_layer_set.geoserver(gso, source = NULL, layers)
  expect_equal(result, 0)
  mockery::expect_called(mock_publish_layer, 2)
})
