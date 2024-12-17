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

test_that("get_layer_categories extracts ALL categories correctly from GeoPackage", {
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")

  categories <- get_layer_categories(from = source_gpkg)

  expect_s3_class(categories, "data.frame")

  expect_true(all(c("id", "description", "color") %in% colnames(categories)))
})


test_that("copy_styles copies styles from GeoPackage to GeoPackage", {
  # Crear un GeoPackage válido como fuente
  source_gpkg <- system.file("extdata", "clc.gpkg", package = "clc")
  dest_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_sf(geometry = sf::st_sfc()), dest_gpkg, quiet = TRUE)

  copy_styles(from = source_gpkg, to = dest_gpkg)

  # Verify the styles have been copied
  dest_styles <- sf::st_read(dest_gpkg, layer = "layer_styles", quiet = TRUE)
  expect_gt(nrow(dest_styles), 0)
})

