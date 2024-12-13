#' Read the style from a source (GeoPackage or PostGIS)
#'
#' Reads the style table from a specified source (GeoPackage or PostGIS).
#'
#' @param source The source to read from (GeoPackage file or PostGIS connection).
#' @param layer_name (Optional) The name of the layer for which the style should
#' be fetched.
#' @return A style object containing the fetched information.
#' @keywords internal
#' @noRd
read_style_from_source <- function(source, layer_name = NULL) {
  style <- suppressWarnings(sf::st_read(source, layer = "layer_styles", quiet = TRUE))

  if (!is.null(layer_name)) {
    style <- style[style$f_table_name == layer_name, ]
    if (nrow(style) == 0) {
      stop("No style found for the specified layer name: ", layer_name)
    }
  } else {
    style <- style[1, ]
  }

  # transform PostGIS style into GeoPackage style
  if ("id" %in% names(style)) {
    names_style_geo <- c(
      "f_table_catalog",
      "f_table_schema",
      "f_table_name",
      "f_geometry_column",
      "styleName",
      "styleQML",
      "styleSLD",
      "useAsDefault",
      "description",
      "owner",
      "ui",
      "update_time"
    )
    style <- style[, tolower(names_style_geo)]
    names(style) <- names_style_geo
    style$f_table_catalog <- ''
    style$f_table_schema <- ''
  }
  return(style)
}

#' Get layers to copy
#'
#' Filters a list of layers, excluding those that are not valid or necessary to copy.
#'
#' @param layers_to_copy (Optional) A vector of layer names to copy.
#' @param all_layers A vector of all available layers.
#' @return A vector of layers that should be copied.
#' @keywords internal
#' @noRd
get_layers_to_copy <- function(layers_to_copy, all_layers) {
  all_layers <- setdiff(all_layers, c("raster_columns", "layer_styles"))
  if (is.null(layers_to_copy)) {
    layers_to_copy <- all_layers
  } else {
    missing_layers <- setdiff(layers_to_copy, all_layers)
    if (length(missing_layers) > 0) {
      stop(
        "The following layers do not exist in the destination: ",
        paste(missing_layers, collapse = ", ")
      )
    }
  }
  layers_to_copy
}

#' Get existing styles
#'
#' Retrieves the existing styles from the target source for a given set of layers.
#'
#' @param to The target source (GeoPackage file or PostGIS connection).
#' @param layers_in_to A vector of layer names available in the target source.
#' @param style A data frame representing the style structure.
#' @return A data frame of existing styles or an empty structure if no styles are present.
#' @keywords internal
#' @noRd
get_existing_styles <- function(to, layers_in_to, style) {
  if ("layer_styles" %in% layers_in_to) {
    existing_styles <- suppressWarnings(sf::st_read(to, layer = "layer_styles", quiet = TRUE))
  } else {
    existing_styles <- style[0, ]  # Create an empty table with the same structure
  }
}

#' Generate new styles for layers
#'
#' Creates a table of styles for the specified layers, optionally associating them
#' with a database or schema.
#'
#' @param layers_to_copy A vector of layer names for which styles will be created.
#' @param style A template data frame representing the base style structure.
#' @param database (Optional) The name of the database to associate with the styles.
#' @param schema (Optional) The name of the schema to associate with the styles.
#' @return A data frame containing the new styles for the specified layers.
#' @keywords internal
#' @noRd
generate_new_styles <- function(layers_to_copy,
                                style,
                                database = NULL,
                                schema = NULL) {
  n <- length(layers_to_copy)
  new_styles <- do.call(rbind, replicate(n, style, simplify = FALSE))

  for (i in seq_along(layers_to_copy)) {
    new_styles$f_table_name[i] <- layers_to_copy[i]
    if (!is.null(schema)) {
      new_styles$f_table_schema[i] <- schema
    }
    if (!is.null(database)) {
      new_styles$f_table_catalog[i] <- database
    }
    new_styles$styleSLD[i] <- gsub(style$f_table_name,
                                   layers_to_copy[i],
                                   new_styles$styleSLD[i],
                                   fixed = TRUE)
  }
  if (!is.null(database)) {
    new_styles$useasdefault <- TRUE
    new_styles$id <- 1
  }
  new_styles
}

#' Combine existing and new styles
#'
#' Merges existing styles with newly generated styles for the layers to copy, and
#' updates the style table in the destination source.
#'
#' @param existing_styles A data frame containing the existing styles.
#' @param new_styles A data frame containing the new styles.
#' @param layers_to_copy A vector of layer names to copy.
#' @param to The target source (GeoPackage file or PostGIS connection).
#' @return A data frame containing the combined styles.
#' @keywords internal
#' @noRd
combine_styles <- function(existing_styles,
                           new_styles,
                           layers_to_copy,
                           to) {
  if ('id' %in% names(new_styles)) {
    names(new_styles) <- tolower(names(new_styles))
    new_styles <- new_styles[, c('id', setdiff(names(new_styles), 'id'))]
  }
  combined_styles <- rbind(existing_styles[!(existing_styles$f_table_name %in% layers_to_copy), ], new_styles)
  if ('id' %in% names(combined_styles)) {
    combined_styles$id <- 1:nrow(combined_styles)
  }
  suppressMessages(
    sf::st_write(
      obj = combined_styles,
      dsn = to,
      layer = "layer_styles",
      append = FALSE,
      quiet = TRUE
    )
  )
  combined_styles
}


#' Get all layers from PostGIS
#'
#' Retrieves all layers with geometric or geographic data from a PostGIS database
#' schema.
#'
#' @param conn A database connection object.
#' @param schema The schema to query for layers.
#' @return A vector of layer names within the specified schema.
#' @keywords internal
#' @noRd
get_all_layers_pg <- function(conn, schema) {
  query <- "
  SELECT
    table_schema AS schema_name,
    table_name,
    column_name AS geometry_column,
    udt_name AS geometry_type
  FROM information_schema.columns
  WHERE udt_name IN ('geometry', 'geography');
"
  all_layers <- RPostgres::dbGetQuery(conn, query)
  all_layers <- all_layers[all_layers$schema_name == schema, "table_name"]
}


#' Check if 'layer_styles' table exists in PostGIS
#'
#' Verifies whether the 'layer_styles' table exists in the specified schema of a
#' PostGIS database.
#'
#' @param conn A database connection object.
#' @param schema The schema to check for the table. Defaults to "public".
#' @return The table name ('layer_styles') if it exists, or NULL otherwise.
#' @keywords internal
#' @noRd
exist_layer_styles_pg <- function(conn, schema = "public") {
  table <- "layer_styles"
  query_check <- sprintf(
    "
    SELECT 1
    FROM information_schema.tables
    WHERE table_name = '%s' AND table_schema = '%s';",
    table,
    schema
  )
  table_exists <- RPostgres::dbGetQuery(conn, query_check)

  if (nrow(table_exists) > 0) {
    table
  } else {
    NULL
  }
}


#' Assign a Style to Layers Based on Layer Name
#'
#' This function copies a style from a source to a destination.
#'
#' By default, it assigns the style to all layers in the destination, but a specific
#' layers can be selected on the target.
#'
#' @param style A data frame representing the style structure.
#' @param to A data destination for the output styles. This can be:
#'   - A string representing the path to a GeoPackage file.
#'   - A `DBI` database connection object to a PostGIS database, created using [RPostgres::dbConnect()].
#' @param database A string, database name, only in case the destination is in PostGIS.
#' @param schema A string, schema name, only in case the destination is in PostGIS.
#'   Defaults to `'public'`.
#' @param layers An optional character vector specifying the names of layers
#'   in the destination to which the styles should be applied.
#'   If `NULL` (default), applies the style to all layers.
#'
#' @return The updated `layer_styles` table, returned invisibly.
#' @keywords internal
#' @noRd
assign_styles_to_layers <- function(style,
                                    to,
                                    database = NULL,
                                    schema = 'public',
                                    layers = NULL) {
  if (is.null(database)) {
    schema <- NULL
    all_layers <- sf::st_layers(to)$name
    layers_in_to <- all_layers
  } else {
    all_layers <- get_all_layers_pg(to, schema)
    layers_in_to <- exist_layer_styles_pg(to, schema)
  }

  layers <- get_layers_to_copy(layers, all_layers)

  existing_styles <- get_existing_styles(to, layers_in_to, style)

  new_styles <- generate_new_styles(layers, style, database, schema)

  combined_styles <- combine_styles(existing_styles, new_styles, layers, to)

  invisible(combined_styles)
}
