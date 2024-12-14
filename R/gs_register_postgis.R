# Function to register a PostGIS database as a datastore in GeoServer
register_postgis_datastore <- function(geoserver_url, workspace, datastore_name, host, port, database, user, password, schema = "public", admin_user = "admin", admin_password = "geoserver") {

  # Define the REST endpoint
  endpoint <- paste0(geoserver_url, "/rest/workspaces/", workspace, "/datastores")

  # Construct the XML payload
  # This contains all the connection parameters needed to configure the PostGIS datastore
  xml_payload <- sprintf('
  <dataStore>
    <name>%s</name>
    <description>PostGIS datastore registered via R</description>
    <type>PostGIS</type>
    <enabled>true</enabled>
    <connectionParameters>
      <entry key="host">%s</entry>
      <entry key="port">%s</entry>
      <entry key="database">%s</entry>
      <entry key="user">%s</entry>
      <entry key="passwd">%s</entry>
      <entry key="dbtype">postgis</entry>
      <entry key="schema">%s</entry>
    </connectionParameters>
  </dataStore>
  ', datastore_name, host, port, database, user, password, schema)

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
    message("PostGIS datastore registered successfully.")
  } else {
    stop(sprintf(
      "Failed to register datastore. Status code: %d\nResponse: %s",
      response$status_code,
      content(response, as = "text", encoding = "UTF-8")
    ))
  }
}



# # Parameters for GeoServer and PostGIS
# geoserver_url <- "http://localhost:8080/geoserver"
# workspace <- "my_workspace"
# datastore_name <- "postgis_datastore"
# host <- "localhost"
# port <- "5432"
# database <- "my_database"
# user <- "my_user"
# password <- "my_password"
# schema <- "my_schema"  # Optional, defaults to "public"
#
# # Call the function
# register_postgis_datastore(
#   geoserver_url = geoserver_url,
#   workspace = workspace,
#   datastore_name = datastore_name,
#   host = host,
#   port = port,
#   database = database,
#   user = user,
#   password = password,
#   schema = schema,
#   admin_user = "admin",        # GeoServer admin user
#   admin_password = "geoserver" # GeoServer admin password
# )
