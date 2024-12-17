#' Register a PostGIS Database as a DataStore in GeoServer
#'
#' Registers a PostGIS database as a `datastore` in a specified GeoServer workspace.
#'
#' If the `datastore` has already been registered previously, there is no need to specify the
#' database connection. For subsequent operations, that `datastore` will be used.
#'
#' In any case, prints an appropriate message.
#'
#' @param gso An object of class `geoserver` containing GeoServer connection details.
#' @param datastore A character string. The name of the datastore to be created.
#' @param db_name A character string. The name of the PostGIS database.
#' @param host A character string. The database host.
#' @param port An integer. The database port (default: 5432).
#' @param db_user A character string. The database username.
#' @param db_password A character string. The database password.
#' @param schema A character string. The database schema (default: "public").
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
#' gso <- gso |>
#'   register_datastore_postgis(
#'     "sigugr-postgis",
#'     db_name = 'sigugr_example',
#'     host = 'localhost',
#'     port = 5432,
#'     db_user = 'user',
#'     db_password = 'password',
#'     schema = "public"
#'   )
#' }
#'
#' @export
register_datastore_postgis <- function(gso, datastore, db_name, host, port, db_user, db_password, schema)
  UseMethod("register_datastore_postgis")


#' @rdname register_datastore_postgis
#' @export
register_datastore_postgis.geoserver <- function(gso,
                                                 datastore,
                                                 db_name,
                                                 host,
                                                 port = 5432,
                                                 db_user,
                                                 db_password,
                                                 schema = "public") {
  # Prepare the body for datastore creation
  datastore_body <- jsonlite::toJSON(list(
    dataStore = list(
      name = datastore,
      type = "PostGIS",
      connectionParameters = list(entry = list(
        list(key = "database", value = db_name),
        list(key = "host", value = host),
        list(key = "port", value = as.character(port)),
        list(key = "user", value = db_user),
        list(key = "passwd", value = db_password),
        list(key = "schema", value = schema),
        list(key = "dbtype", value = "postgis")
      ))
    )
  ), auto_unbox = TRUE)

  register_datastore(gso, datastore, datastore_body)
}




#' Register a Datastore in GeoServer
#'
#' Registers a datastore (such as PostGIS or GeoPackage) in GeoServer.
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
    datastore_url <- paste0(gso$url,
                            "/rest/workspaces/",
                            gso$workspace,
                            "/datastores")
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
