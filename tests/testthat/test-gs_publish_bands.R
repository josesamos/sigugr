test_that("publish_bands.geoserver handles file not found error", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  expect_error(
    publish_bands.geoserver(gso, "nonexistent_file.tif"),
    "The specified raster file does not exist"
  )
})


# Test successful publication of bands
test_that("publish_bands.geoserver publishes all bands successfully", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  source_tif <- system.file("extdata/sat.tif", package = "sigugr")

  mock_publish_raster <- mockery::mock(0, cycle = TRUE)

  mockery::stub(publish_bands.geoserver, "publish_raster", mock_publish_raster)

  result <- publish_bands.geoserver(gso, source_tif)

  expect_equal(result, 0)
  mockery::expect_called(mock_publish_raster, 6)
})


# Test band indices out of bounds
test_that("publish_bands.geoserver handles out-of-bounds band indices", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  source_tif <- system.file("extdata/sat.tif", package = "sigugr")

  expect_error(
    publish_bands.geoserver(gso, source_tif, bands = c(1, 7)),
    "Some band indices are out of bounds"
  )
})


# Test publishing specific bands
test_that("publish_bands.geoserver publishes selected bands", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  source_tif <- system.file("extdata/sat.tif", package = "sigugr")

  mock_publish_raster <- mockery::mock(0, cycle = TRUE)

  mockery::stub(publish_bands.geoserver, "publish_raster", mock_publish_raster)

  result <- publish_bands.geoserver(gso, source_tif, bands = c(1, 3))

  expect_equal(result, 0)
  mockery::expect_called(mock_publish_raster, 2)
})

test_that("publish_bands.geoserver returns error when bands are not published", {
  # Configurar objeto gso simulado
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  # Crear raster de prueba
  source_tif <- system.file("extdata/sat.tif", package = "sigugr")

  # Mock para publish_raster simulando fallo en publicación
  mock_publish_raster <- mockery::mock(1) # Simula fallo (1 = error)

  # Sustituir publish_raster por el mock
  mockery::stub(publish_bands.geoserver, "publish_raster", mock_publish_raster)

  # Ejecutar la función y comprobar el mensaje de error
  expect_message(
    result <- publish_bands.geoserver(gso, source_tif),
    regexp = "Not all available bands have been published\\."
  )

  # Comprobar que la función devuelve un resultado de error
  expect_equal(result, 1)

  # Comprobar que publish_raster fue llamado al menos una vez
  mockery::expect_called(mock_publish_raster, 1)
})
