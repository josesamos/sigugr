test_that("clip_layer handles intersecting geometries", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:3,
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), sf::st_point(c(1.5, 1.5)), sf::st_point(c(2.5, 2.5)))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
  )))), crs = 4326)

  result <- clip_layer(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
})

test_that("clip_layer handles disjoint geometries", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:3,
    geometry = sf::st_sfc(sf::st_point(c(10, 10)), sf::st_point(c(11, 11)), sf::st_point(c(12, 12)))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
    )))
  ), crs = 4326)

  result <- clip_layer(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 0)
})

test_that("clip_layer handles CRS transformations", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:3,
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), sf::st_point(c(1.5, 1.5)), sf::st_point(c(2.5, 2.5)))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
  )))), crs = 4326)
  polygon <- sf::st_transform(polygon, crs = 32630)

  result <- clip_layer(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_equal(sf::st_crs(result), sf::st_crs(polygon))
})


test_that("clip_multipoligon handles correct clipping", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
    )), sf::st_polygon(list(
      rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))
    )))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))
    )))
  ), crs = 4326)

  result <- clip_multipoligon(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
})

test_that("clip_multipoligon handles invalid MULTIPOLYGON encoding",
          {
            vector <- sf::st_as_sf(data.frame(
              id = 1:2,
              geometry = sf::st_sfc(
                sf::st_geometrycollection(list(sf::st_polygon(list(
                  rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
                )))),
                sf::st_geometrycollection(list(sf::st_polygon(list(
                  rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))
                ))))
              )
            ), crs = 4326)

            polygon <- sf::st_as_sf(data.frame(
              id = 1,
              geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))
              )))
            ), crs = 4326)

            result <- clip_multipoligon(vector, polygon)

            expect_s3_class(result, "sf")
            expect_equal(nrow(result), 2)
          })

test_that("clip_multipoligon handles CRS transformations", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
    )), sf::st_polygon(list(
      rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))
    )))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
  )))), crs = 4326)
  polygon <- sf::st_transform(polygon, crs = 32630)

  result <- clip_multipoligon(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_equal(sf::st_crs(result), sf::st_crs(polygon))
})

test_that("clip_multipoligon returns empty for disjoint geometries",
          {
            vector <- sf::st_as_sf(data.frame(
              id = 1:2,
              geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(10, 10), c(12, 10), c(12, 12), c(10, 12), c(10, 10))
              )), sf::st_polygon(list(
                rbind(c(15, 15), c(18, 15), c(18, 18), c(15, 18), c(15, 15))
              )))
            ), crs = 4326)

            polygon <- sf::st_as_sf(data.frame(
              id = 1,
              geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
              )))
            ), crs = 4326)

            result <- clip_multipoligon(vector, polygon)

            expect_s3_class(result, "sf")
            expect_equal(nrow(result), 0)
          })


test_that("clip_layer works correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")

  clc <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
  lanjaron <- sf::st_read(gpkg_path, layer = "lanjaron", quiet = TRUE)

  clipped <- clip_layer(clc, lanjaron)

  expect_s3_class(clipped, "sf")

  expect_equal(colnames(clipped), colnames(clc))
})

test_that("clip_multipoligon works correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
  clc <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
  lanjaron <- sf::st_read(gpkg_path, layer = "lanjaron", quiet = TRUE)

  clc_non_multipolygon <- suppressWarnings(sf::st_cast(clc, "POLYGON"))

  clipped <- clip_multipoligon(clc_non_multipolygon, lanjaron)

  expect_s3_class(clipped, "sf")

  expect_equal(colnames(clipped), colnames(clc))
})

test_that("clip_layer handles different CRS correctly", {
  gpkg_path <- system.file("extdata", "clc.gpkg", package = "clc")
  clc <- sf::st_read(gpkg_path, layer = "clc", quiet = TRUE)
  lanjaron <- sf::st_read(gpkg_path, layer = "lanjaron", quiet = TRUE)

  lanjaron_transformed <- sf::st_transform(lanjaron, 3857)

  clipped <- clip_layer(clc, lanjaron_transformed)

  expect_equal(sf::st_crs(clipped), sf::st_crs(lanjaron_transformed))
})


test_that("clip_layer works correctly with LINESTRING geometries", {
  # Create a simple LINESTRING object
  lines <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 1))),
                  sf::st_linestring(rbind(c(2, 2), c(3, 3))),
                  crs = 4326)
  vector <- sf::st_sf(id = 1:2, geometry = lines)

  # Create a simple POLYGON for clipping
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))), crs = 4326)
  polygon <- sf::st_sf(id = 1, geometry = polygon)

  # Run clip_layer
  result <- clip_layer(vector, polygon)

  # Assertions
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1) # Only one LINESTRING overlaps
  expect_true(all(sf::st_geometry_type(result) == "MULTILINESTRING"))
})

test_that("clip_layer works correctly with POINT geometries", {
  # Create a simple POINT object
  points <- sf::st_sfc(sf::st_point(c(0.5, 0.5)),
                   sf::st_point(c(1.5, 1.5)),
                   sf::st_point(c(3, 3)),
                   crs = 4326)
  vector <- sf::st_sf(id = 1:3, geometry = points)

  # Create a simple POLYGON for clipping
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))), crs = 4326)
  polygon <- sf::st_sf(id = 1, geometry = polygon)

  # Run clip_layer
  result <- clip_layer(vector, polygon)

  # Assertions
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2) # Two points overlap with the polygon
  expect_true(all(sf::st_geometry_type(result) == "MULTIPOINT"))
})

test_that("clip_layer throws an error for mixed geometry types", {
  # Create a simple mixed geometry object
  geometries <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_linestring(rbind(c(0, 0), c(1, 1))),
    sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2))))
  )
  vector <- sf::st_sf(id = 1:3, geometry = geometries, crs = 4326)

  # Create a simple POLYGON for clipping
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))), crs = 4326)
  polygon <- sf::st_sf(id = 1, geometry = polygon)

  # Run clip_layer
  result <- clip_layer(vector, polygon)

  # Assertions
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
  expect_true(all(sf::st_geometry_type(result) == "MULTIPOLYGON"))
})


################

test_that("clip_layer handles POLYGON and MULTIPOLYGON geometries", {
  # Create test data: a valid POLYGON and MULTIPOLYGON geometry
  polygon <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))),
    crs = 4326
  )
  multipolygon <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_multipolygon(list(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))))),
    crs = 4326
  )

  # Test that POLYGON is converted to MULTIPOLYGON
  res <- clip_layer(polygon, polygon)
  expect_equal(as.character(unique(sf::st_geometry_type(res))), "MULTIPOLYGON")

  # Test that MULTIPOLYGON remains unchanged
  res <- clip_layer(multipolygon, polygon)
  expect_equal(as.character(unique(sf::st_geometry_type(res))), "MULTIPOLYGON")
})

test_that("clip_layer handles LINESTRING and MULTILINESTRING geometries", {
  # Create test data: valid LINESTRING and MULTILINESTRING geometries
  linestring <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  multilinestring <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_multilinestring(list(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)))),
    crs = 4326
  )
  polygon <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(-1, -1, 2, -1, 2, 2, -1, 2, -1, -1), ncol = 2, byrow = TRUE)))),
    crs = 4326
  )

  # Test that LINESTRING is converted to MULTILINESTRING
  res <- clip_layer(linestring, polygon)
  expect_equal(as.character(unique(sf::st_geometry_type(res))), "MULTILINESTRING")

  # Test that MULTILINESTRING remains unchanged
  res <- clip_layer(multilinestring, polygon)
  expect_equal(as.character(unique(sf::st_geometry_type(res))), "MULTILINESTRING")
})

test_that("clip_layer handles POINT and MULTIPOINT geometries", {
  # Create test data: valid POINT and MULTIPOINT geometries
  point <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_point(c(0, 0))),
    crs = 4326
  )
  multipoint <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_multipoint(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  polygon <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(-1, -1, 2, -1, 2, 2, -1, 2, -1, -1), ncol = 2, byrow = TRUE)))),
    crs = 4326
  )

  # Test that POINT is converted to MULTIPOINT
  res <- clip_layer(point, polygon)
  expect_equal(as.character(unique(sf::st_geometry_type(res))), "MULTIPOINT")

  # Test that MULTIPOINT remains unchanged
  res <- clip_layer(multipoint, polygon)
  expect_equal(as.character(unique(sf::st_geometry_type(res))), "MULTIPOINT")
})

test_that("clip_layer filters mixed LINESTRING and POINT geometries", {
  # Create test data: valid LINESTRING and POINT geometries
  linestring <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  point <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_point(c(2, 2))),
    crs = 4326
  )
  mixed <- rbind(linestring, point)
  polygon <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(-1, -1, 3, -1, 3, 3, -1, 3, -1, -1), ncol = 2, byrow = TRUE)))),
    crs = 4326
  )

  # Test that only LINESTRING/MULTILINESTRING geometries are retained
  res <- clip_layer(mixed, polygon)
  expect_equal(as.character(unique(sf::st_geometry_type(res))), "MULTILINESTRING")
})

