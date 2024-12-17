test_that("store_raster correctly stores a raster file to PostGIS", {
  # Define un archivo raster temporal simulado
  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(terra::rast(nrows = 10, ncols = 10, nlyrs = 1, vals = runif(100)),
                     filename = temp_raster, overwrite = TRUE)

  # Mock para rpostgis::pgWriteRast
  mock_pgWriteRast <- mockery::mock(NULL)

  # Mock para la conexión DB
  mock_conn <- mockery::mock("DB Connection")

  # Stub rpostgis::pgWriteRast
  mockery::stub(sigugr::store_raster, "rpostgis::pgWriteRast", mock_pgWriteRast)

  # Ejecuta la función
  result <- sigugr::store_raster(temp_raster, conn = mock_conn, schema = "test_schema", table_name = "test_raster")

  # Comprobaciones
  expect_equal(result, "test_raster")  # El nombre de la tabla debe coincidir
  mockery::expect_called(mock_pgWriteRast, 1)  # Asegura que se llamó 1 vez
})

test_that("store_raster derives table name from raster file when table_name is NULL", {
  # Define un archivo raster temporal
  temp_raster <- tempfile(fileext = ".tif")
  terra::writeRaster(terra::rast(nrows = 10, ncols = 10, nlyrs = 1, vals = runif(100)),
                     filename = temp_raster, overwrite = TRUE)

  # Mock para rpostgis::pgWriteRast
  mock_pgWriteRast <- mockery::mock(NULL)

  # Mock de la conexión
  mock_conn <- mockery::mock("DB Connection")

  # Stub para rpostgis::pgWriteRast
  mockery::stub(sigugr::store_raster, "rpostgis::pgWriteRast", mock_pgWriteRast)

  # Ejecuta sin proporcionar un table_name
  result <- sigugr::store_raster(temp_raster, conn = mock_conn, schema = "test_schema")

  # Deriva el nombre del archivo sin extensión y lo convierte a snake_case
  expected_table_name <- snakecase::to_snake_case(tools::file_path_sans_ext(basename(temp_raster)))

  # Comprobaciones
  expect_equal(result, expected_table_name)
  mockery::expect_called(mock_pgWriteRast, 1)
})

test_that("store_raster stops if the raster file does not exist", {
  # Define una ruta inexistente
  non_existent_raster <- "fake/path/to/nonexistent.tif"

  # Mock para conexión
  mock_conn <- mockery::mock("DB Connection")

  # Verifica que se lanza un error si el archivo no existe
  expect_error(
    sigugr::store_raster(non_existent_raster, conn = mock_conn),
    "The specified raster file does not exist"
  )
})
