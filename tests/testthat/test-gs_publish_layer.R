test_that("publish_layer.geoserver works when layer already exists (200)", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  layer <- "test_layer"

  mock_get_200 <- mockery::mock(
    structure(list(status_code = 200), class = "response")
  )
  mockery::stub(publish_layer.geoserver, "httr::GET", mock_get_200)

  expect_message(
    result <- publish_layer.geoserver(gso, layer),
    sprintf("Layer %s already exists.", layer)
  )
  expect_equal(result, 0)
  mockery::expect_called(mock_get_200, 1)
})


test_that("publish_layer.geoserver publishes layer successfully (201)", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  layer <- "new_layer"

  mock_get_404 <- mockery::mock(
    structure(list(status_code = 404), class = "response")
  )
  mock_post_201 <- mockery::mock(
    structure(list(status_code = 201), class = "response")
  )

  mock_tojson <- mockery::mock('{"featureType":{"name":"new_layer","nativeName":"new_layer","title":"new_layer"}}')

  mockery::stub(publish_layer.geoserver, "httr::GET", mock_get_404)
  mockery::stub(publish_layer.geoserver, "httr::POST", mock_post_201)
  mockery::stub(publish_layer.geoserver, "jsonlite::toJSON", mock_tojson)

  expect_message(
    result <- publish_layer.geoserver(gso, layer),
    sprintf("Layer %s published successfully.", layer)
  )
  expect_equal(result, 0)
  mockery::expect_called(mock_get_404, 1)
  mockery::expect_called(mock_post_201, 1)
  mockery::expect_called(mock_tojson, 1)
})


test_that("publish_layer.geoserver handles failed publication (error response)", {
  # Define a GeoServer object
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  layer <- "example_layer"

  # Mock httr::GET to return 404 (layer does not exist)
  mock_get_404 <- mockery::mock(
    structure(list(status_code = 404), class = "response")
  )

  # Mock httr::POST to return 500 with a dummy error message
  mock_post_error <- mockery::mock(
    structure(
      list(
        status_code = 500,
        content = charToRaw("Internal Server Error")
      ),
      class = "response"
    )
  )

  # Stub the GET and POST calls
  mockery::stub(publish_layer.geoserver, "httr::GET", mock_get_404)
  mockery::stub(publish_layer.geoserver, "httr::POST", mock_post_error)

  # Check for correct error message and return value
  expect_message(
    result <- publish_layer.geoserver(gso, layer),
    "Failed to publish layer example_layer. Error: Internal Server Error"
  )
  expect_equal(result, 1)

  # Verify mock calls
  mockery::expect_called(mock_get_404, 1)
  mockery::expect_called(mock_post_error, 1)
})
