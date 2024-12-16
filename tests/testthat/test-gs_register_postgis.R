test_that("register_datastore_postgis.geoserver successfully registers a PostGIS datastore", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test"
  )
  class(gso) <- "geoserver"

  mock_register <- mockery::mock(gso)

  mockery::stub(register_datastore_postgis.geoserver, "register_datastore", mock_register)

  result <- register_datastore_postgis.geoserver(
    gso,
    datastore = "postgis_datastore",
    db_name = "test_db",
    host = "localhost",
    port = 5432,
    db_user = "dbuser",
    db_password = "dbpassword"
  )

  expect_identical(result, gso)
  mockery::expect_called(mock_register, 1)
})

test_that("register_datastore handles existing datastore (GET 200)", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test"
  )
  class(gso) <- "geoserver"

  mock_get_200 <- mockery::mock(
    structure(
      list(status_code = 200),
      class = "response"
    )
  )

  mockery::stub(register_datastore, "httr::GET", mock_get_200)

  gso_expected <- gso
  gso_expected$datastore <- "existing_datastore"

  expect_message(
    result <- register_datastore(gso, "existing_datastore", "{}"),
    "Datastore already exists."
  )
  expect_identical(result, gso_expected)
  mockery::expect_called(mock_get_200, 1)
})

test_that("register_datastore successfully registers a datastore (POST 201)", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test"
  )
  class(gso) <- "geoserver"

  mock_get_404 <- mockery::mock(
    structure(
      list(status_code = 404),
      class = "response"
    )
  )
  mock_post_201 <- mockery::mock(
    structure(
      list(status_code = 201),
      class = "response"
    )
  )

  mockery::stub(register_datastore, "httr::GET", mock_get_404)
  mockery::stub(register_datastore, "httr::POST", mock_post_201)

  gso_expected <- gso
  gso_expected$datastore <- "new_datastore"

  expect_message(
    result <- register_datastore(gso, "new_datastore", "{}"),
    "Datastore successfully registered!"
  )
  expect_identical(result, gso_expected)
  mockery::expect_called(mock_get_404, 1)
  mockery::expect_called(mock_post_201, 1)
})



test_that("register_datastore handles error when datastore registration fails (POST != 201)", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test"
  )
  class(gso) <- "geoserver"

  mock_get_404 <- mockery::mock(
    structure(
      list(status_code = 404),
      class = "response"
    )
  )
  mock_post_error <- mockery::mock(
    structure(
      list(
        status_code = 500,
        content = charToRaw("Internal Server Error"),
        url = "http://example.com/geoserver/rest/workspaces/sigugr_test/datastores"
      ),
      class = "response"
    )
  )

  mockery::stub(register_datastore, "httr::GET", mock_get_404)
  mockery::stub(register_datastore, "httr::POST", mock_post_error)

  expect_message(
    result <- register_datastore(gso, "error_datastore", "{}"),
    regexp = "^Error registering datastore"
  )
  expect_null(result)
  mockery::expect_called(mock_get_404, 1)
  mockery::expect_called(mock_post_error, 1)
})

