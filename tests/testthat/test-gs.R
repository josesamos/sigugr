test_that("geoserver create object", {
  # Initialize gso
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  # Mock check_and_create_workspace to return the same gso object
  mock_check_and_create_workspace <- mockery::mock(gso)
  mockery::stub(geoserver, "check_and_create_workspace", mock_check_and_create_workspace)

  # Call function
  result <- geoserver(
    url = "http://localhost:8080/geoserver",
    user = "admin",
    password = "geoserver",
    workspace = "sigugr_test"
  )

  # Assertions
  testthat::expect_s3_class(result, "geoserver")
  mockery::expect_called(mock_check_and_create_workspace, 1)
})

test_that("check_and_create_workspace works as expected", {
  # Create a mock GeoServer object
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  # Case 1: Workspace already exists (status 200)
  mock_get_200 <- mockery::mock(
    structure(
      list(status_code = 200),
      class = "response"
    )
  )
  mock_status_code_200 <- mockery::mock(200)

  # Stub httr functions with mocks
  mockery::stub(check_and_create_workspace, "httr::GET", mock_get_200)
  mockery::stub(check_and_create_workspace, "httr::status_code", mock_status_code_200)

  # Call function and check behavior
  testthat::expect_message(
    result <- check_and_create_workspace(gso),
    "The workspace already exists."
  )
  testthat::expect_equal(result, gso)

  # Ensure mocks are called correctly
  mockery::expect_called(mock_get_200, 1)
  mockery::expect_called(mock_status_code_200, 1)
})


test_that("check_and_create_workspace works when workspace exists (200)", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  mock_get_200 <- mockery::mock(
    structure(list(status_code = 200), class = "response")
  )
  mockery::stub(check_and_create_workspace, "httr::GET", mock_get_200)

  expect_message(
    result <- check_and_create_workspace(gso),
    "The workspace already exists."
  )
  expect_equal(result, gso)
  mockery::expect_called(mock_get_200, 1)
})

test_that("check_and_create_workspace works when workspace does not exist (404) and is created (201)", {
  gso <- list(
    url = "http://example.com/geoserver",
    user = "admin",
    password = "password",
    workspace = "sigugr_test",
    datastore = "datastore"
  )
  class(gso) <- "geoserver"

  mock_get_404 <- mockery::mock(
    structure(list(status_code = 404), class = "response")
  )
  mock_post_201 <- mockery::mock(
    structure(list(status_code = 201), class = "response")
  )

  mockery::stub(check_and_create_workspace, "httr::GET", mock_get_404)
  mockery::stub(check_and_create_workspace, "httr::POST", mock_post_201)

  expect_message(
    result <- check_and_create_workspace(gso),
    "Workspace successfully created!"
  )
  expect_equal(result, gso)
  mockery::expect_called(mock_get_404, 1)
  mockery::expect_called(mock_post_201, 1)
})

