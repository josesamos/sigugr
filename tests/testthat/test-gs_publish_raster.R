test_that("publish_raster.geoserver handles non-existent raster file", {
  # Define a GeoServer object
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  raster <- "nonexistent_file.tif"

  # Expect an error when the raster file does not exist
  expect_error(
    publish_raster.geoserver(gso, raster),
    paste("The specified raster file does not exist:", raster)
  )
})

test_that("publish_raster.geoserver publishes raster successfully", {
  # Define un objeto GeoServer
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  raster <- "example_raster.tif"

  # Mockear file.exists para retornar TRUE
  mock_file_exists <- mockery::mock(TRUE)
  mockery::stub(publish_raster.geoserver, "file.exists", mock_file_exists)

  # Mockear httr::upload_file para retornar un objeto simulado
  mock_upload_file <- mockery::mock(raster)
  mockery::stub(publish_raster.geoserver, "httr::upload_file", mock_upload_file)

  # Mockear httr::PUT para simular una publicación exitosa
  mock_put_success <- mockery::mock(
    structure(list(status_code = 201), class = "response")
  )
  mockery::stub(publish_raster.geoserver, "httr::PUT", mock_put_success)

  # Verificar el mensaje de éxito y el resultado
  expect_message(
    result <- publish_raster.geoserver(gso, raster),
    "Raster example_raster published successfully."
  )
  expect_equal(result, 0)

  # Verificar llamadas a los mocks
  mockery::expect_called(mock_file_exists, 1)
  mockery::expect_called(mock_upload_file, 1)
  mockery::expect_called(mock_put_success, 1)
})

test_that("publish_raster.geoserver handles publication error", {
  # Define un objeto GeoServer
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  raster <- "example_raster.tif"

  # Mockear file.exists para retornar TRUE
  mock_file_exists <- mockery::mock(TRUE)
  mockery::stub(publish_raster.geoserver, "file.exists", mock_file_exists)

  # Mockear httr::upload_file para retornar un objeto simulado
  mock_upload_file <- mockery::mock(raster)
  mockery::stub(publish_raster.geoserver, "httr::upload_file", mock_upload_file)

  # Mockear httr::PUT para simular un error HTTP
  mock_put_error <- mockery::mock(
    structure(
      list(
        status_code = 500,
        content = charToRaw("Internal Server Error"),
        url = "http://example.com/geoserver/rest/workspaces/sigugr_test/coveragestores/example_raster/file.geotiff"
      ),
      class = "response"
    )
  )
  mockery::stub(publish_raster.geoserver, "httr::PUT", mock_put_error)

  # Verificar el mensaje de error y el resultado
  expect_message(
    result <- publish_raster.geoserver(gso, raster),
    "Failed to publish raster example_raster. Error: Internal Server Error"
  )
  expect_equal(result, 1)

  # Verificar llamadas a los mocks
  mockery::expect_called(mock_file_exists, 1)
  mockery::expect_called(mock_upload_file, 1)
  mockery::expect_called(mock_put_error, 1)
})
