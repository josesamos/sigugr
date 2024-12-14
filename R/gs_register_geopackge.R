# Function to register a GeoPackage as a datastore in GeoServer
register_geopackage_datastore <- function(geoserver_url, workspace, datastore_name, geopackage_path, admin_user = "admin", admin_password = "geoserver") {

  # Define the REST endpoint
  endpoint <- paste0(geoserver_url, "/rest/workspaces/", workspace, "/datastores")

  # Construct the XML payload
  # This specifies the path to the GeoPackage file
  xml_payload <- sprintf('
  <dataStore>
    <name>%s</name>
    <description>GeoPackage datastore registered via R</description>
    <type>GeoPackage</type>
    <enabled>true</enabled>
    <connectionParameters>
      <entry key="database">%s</entry>
    </connectionParameters>
  </dataStore>
  ', datastore_name, geopackage_path)

  # Send the POST request to GeoServer
  response <- POST(
    url = endpoint,
    authenticate(admin_user, admin_password),  # Admin credentials
    body = xml_payload,                        # XML payload
    encode = "raw",                            # Prevent automatic content encoding
    content_type("application/xml")            # Content-Type header
  )

  # Check the response status
  if (response$status_code %in% c(200, 201)) {
    message("GeoPackage datastore registered successfully.")
  } else {
    stop(sprintf(
      "Failed to register datastore. Status code: %d\nResponse: %s",
      response$status_code,
      content(response, as = "text", encoding = "UTF-8")
    ))
  }
}



# # Parameters for GeoServer and GeoPackage
# geoserver_url <- "http://localhost:8080/geoserver"
# workspace <- "my_workspace"
# datastore_name <- "geopackage_datastore"
# geopackage_path <- "/path/to/your/geopackage.gpkg"  # Absolute path to the GeoPackage file
#
# # Call the function
# register_geopackage_datastore(
#   geoserver_url = geoserver_url,
#   workspace = workspace,
#   datastore_name = datastore_name,
#   geopackage_path = geopackage_path,
#   admin_user = "admin",        # GeoServer admin user
#   admin_password = "geoserver" # GeoServer admin password
# )
