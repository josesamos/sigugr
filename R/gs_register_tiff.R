#' Register a TIFF as a DataStore in GeoServer
#'
#' This function registers a TIFF as a `datastore` in a specified GeoServer workspace
#' using the REST API.
#'
#' If the `datastore` has already been registered previously, there is no need to specify the
#' TIFF. For subsequent operations, that `datastore` will be used.
#'
#' In any case, prints an appropriate message.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param datastore A character string. The name of the datastore to be created.
#' @param tiff A character string specifying the full file path to the TIFF
#'   file. The default is `NULL`, which assumes the datastore has already been registered.
#'
#' @return An object of class `geoserver` or NULL if an error occurred.
#'
#' @family publish to GeoServer
#'
#' @examples
#' \dontrun{
#' gso <- geoserver(
#'   url = "http://localhost:8080/geoserver",
#'   user = "admin",
#'   password = "geoserver",
#'   workspace = "sigugr_test"
#' )
#'
#' source_tif <- system.file("extdata/sat.tif", package = "sigugr")
#'
#' gso <- gso |>
#'   register_datastore_tiff("sigugr-tiff", source_tif)
#' }
#'
#' @export
register_datastore_tiff <- function(gso, datastore, tiff)
  UseMethod("register_datastore_tiff")


#' @rdname register_datastore_tiff
#' @export
register_datastore_tiff.geoserver <- function(gso, datastore, tiff = NULL) {
  # Check if TIFF file exists
  if (!file.exists(tiff)) {
    stop("The provided TIFF file path does not exist.")
  }

  # Define URL
  datastore_url <- paste0(gso$url,
                          "/rest/workspaces/",
                          gso$workspace,
                          "/coveragestores")

  # Prepare the body for datastore creation
  datastore_body <- jsonlite::toJSON(list(
    coverageStore = list(
      name = datastore,
      type = "GeoTIFF",
      url = paste0("file://", tiff),
      workspace = gso$workspace,
      enabled = TRUE
    )
  ), auto_unbox = TRUE)

  register_datastore(gso, datastore, datastore_body, datastore_url)
}



#' Register a Datastore in GeoServer
#'
#' This function registers a datastore (such as PostGIS or GeoPackage) in GeoServer.
#' It first checks if the datastore already exists and, if not, registers it using
#' the provided parameters.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param datastore A character string. The name of the datastore to register.
#' @param datastore_body A list containing the JSON body for the datastore configuration.
#' @param datastore_url (Optional) A character string. The full URL for registering the
#'   datastore.
#'
#' @return An object of class `geoserver` or NULL if an error occurred.
#' @details
#' The function first checks if the datastore already exists in the specified workspace.
#' If it does, a message is printed, and no further action is taken. Otherwise, the function
#' sends a \code{POST} request to register the datastore using the provided body configuration.
#'
#' @keywords internal
#' @noRd
register_datastore <- function(gso,
                               datastore,
                               datastore_body,
                               datastore_url = NULL) {
  gso$datastore <- datastore

  if (is.null(datastore_url)) {
    datastore_url <- paste0(gso$url, "/rest/workspaces/", gso$workspace, "/datastores")
  }

  # Define URL
  datastore_check_url <- paste0(datastore_url, "/", datastore)

  # Check if the datastore already exists
  check_response <- httr::GET(url = datastore_check_url, httr::authenticate(gso$user, gso$password))

  if (httr::status_code(check_response) == 200) {
    message("Datastore already exists.")
    return(gso)
  }

  # Register datastore
  response <- httr::POST(
    url = datastore_url,
    httr::authenticate(gso$user, gso$password),
    body = datastore_body,
    encode = "json",
    httr::content_type_json()
  )

  if (httr::status_code(response) == 201) {
    message("Datastore successfully registered!")
    return(gso)
  } else {
    message("Error registering datastore: ",
            httr::content(response, "text"))
    return(NULL)
  }
}
