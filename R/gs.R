#' GeoServer Connection Object (`geoserver` S3 Class)
#'
#' This S3 class represents a connection to a GeoServer instance.
#' It stores the connection details, including the base URL, user credentials,
#' and the default workspace.
#'
#' @param url A character string specifying the base URL of the GeoServer instance
#'   (e.g., `"http://localhost:8080/geoserver"`).
#' @param user A character string representing the GeoServer username with the
#'   required permissions.
#' @param password A character string representing the password for the specified user.
#' @param workspace A character string specifying the default workspace to use in
#'   GeoServer operations.
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
#' }
#'
#' @export
geoserver <- function(url, user, password, workspace) {
  gso <- list(
    url = url,
    user = user,
    password = password,
    workspace = workspace,
    datastore = ''
  )

  class(gso) <- "geoserver"

  check_and_create_workspace(gso)
}


#' Check and Create a Workspace in GeoServer
#'
#' This function checks if a workspace exists in GeoServer using its REST API.
#' If the workspace does not exist, it creates it.
#'
#' @keywords internal
#' @noRd
check_and_create_workspace <- function(gso) {
  # Define the REST API endpoints
  query_url <- paste0(gso$url, "/rest/workspaces/", gso$workspace)
  create_url <- paste0(gso$url, "/rest/workspaces")

  # Check if the workspace exists
  response_get <- httr::GET(url = query_url,
                            httr::authenticate(gso$user, gso$password),
                            httr::content_type_json())

  if (httr::status_code(response_get) == 200) {
    message("The workspace already exists.")
    return(gso)
  } else if (httr::status_code(response_get) == 404) {
    # Prepare the body for workspace creation
    workspace_body <- jsonlite::toJSON(list(workspace = list(name = gso$workspace)), auto_unbox = TRUE)

    # Create the workspace
    response_post <- httr::POST(
      url = create_url,
      httr::authenticate(gso$user, gso$password),
      body = workspace_body,
      encode = "json",
      httr::content_type_json()
    )

    if (httr::status_code(response_post) == 201) {
      message("Workspace successfully created!")
      return(gso)
    } else {
      message("Error creating the workspace: ",
              httr::content(response_post, "text"))
      return(NULL)
    }
  } else {
    message("Error checking the workspace: ",
            httr::content(response_get, "text"))
    return(NULL)
  }
}

