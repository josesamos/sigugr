#' Copy styles layer from one source `GeoPackage` to another
#'
#' Assigns the first style from the source to all layers in the destination.
#'
#' @param from A GeoPackage file name.
#' @param to A GeoPackage file name.
#'
#' @return `obj`, invisibly.
#'
#' @family transformation functions
#'
#' @examples
#' #
#'
#' @export
copy_styles_layer <- function(from, to) {
  layer <- "layer_styles"
  style <- sf::st_read(from, layer = layer, quiet = TRUE)
  style <- style[1, ]

  layers <- sf::st_layers(to)
  my_style <- style
  n <- length(layers$name)
  if (n > 1) {
    for (i in 2:n) {
      my_style <- rbind(my_style, style)
    }
  }
  for (i in 1:n) {
    my_style$f_table_name[i] <- layers$name[i]
    gsub(style$f_table_name, layers$name[i], my_style$styleSLD[i], fixed = TRUE)
  }
  sf::st_write(
    obj = my_style,
    dsn = to,
    layer = layer,
    append = FALSE,
    quiet = TRUE
  )
}


#' Copy styles layer from one source `GeoPackage` to a PostGIS database.
#'
#' Assigns the first style from the source the given layers in the destination.
#'
#' @param from A GeoPackage file name.
#' @param to A database connection.
#' @param layers A vector of layer names.
#' @param database A string, database name.
#' @param schema A string, schema name.
#'
#' @return `obj`, invisibly.
#'
#' @family transformation functions
#'
#' @examples
#' #
#'
#' @export
copy_styles_layer_names <- function(from, to, layers, database, schema='public') {
  layer <- "layer_styles"
  style <- sf::st_read(from, layer = layer, quiet = TRUE)
  style <- style[1, ]

  my_style <- style
  n <- length(layers)
  if (n > 1) {
    for (i in 2:n) {
      my_style <- rbind(my_style, style)
    }
  }

  my_style$id <- 1:nrow(my_style)
  my_style <- my_style[, c("id", names(my_style)[-length(names(my_style))])]
  names(my_style) <- tolower(names(my_style))

  for (i in 1:n) {
    my_style$f_table_name[i] <- layers[i]
    my_style$f_table_schema[i] <- schema
    my_style$f_table_catalog[i] <- database
    gsub(style$f_table_name, layers[i], my_style$stylesld[i], fixed = TRUE)
  }
  my_style$useasdefault <- TRUE

  sf::st_write(
    obj = my_style,
    dsn = to,
    layer = layer,
    append = FALSE,
    quiet = TRUE
  )
}


#' Get layer categories from a styles layer from one source `GeoPackage`
#'
#' It obtains them from the first defined style.
#'
#' @param from A GeoPackage file name.
#' @param values A vector of integers or NULL.
#'
#' @return `categories` A data freame of categories.
#'
#' @family transformation functions
#'
#' @examples
#' #
#'
#' @export
get_layer_categories <- function(from, values) {
  layer <- "layer_styles"
  style <- sf::st_read(from, layer = layer, quiet = TRUE)
  style <- style[1, ]
  st_xml <- xml2::read_xml(style$styleQML[1])

  categories <- xml2::xml_find_all(st_xml, "//category")
  id <- as.integer(xml2::xml_attr(categories, "value"))
  des <- xml2::xml_attr(categories, "label")
  symbol <- xml2::xml_attr(categories, "symbol")

  s <- xml2::xml_find_all(st_xml, ".//symbols/symbol")
  name <- xml2::xml_attr(s, "name")
  color <- xml2::xml_find_first(s, ".//prop[@k='color']") |> xml2::xml_attr("v")

  rgb2hex <- function(color) {
    rgb <- strsplit(color, ",")
    rgb <- rgb[[1]]
    rgb <- as.numeric(rgb)
    rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
  }
  color2 <- sapply(color, rgb2hex)

  names(color2) <- name
  color2 <- color2[order(as.numeric(names(color2)))]

  if (!is.null(values)) {
    des <- des[id %in% values]
    color2 <- color2[id %in% values]
    id <- id[id %in% values]
  }
  categories <- data.frame(
    ID = id,
    Descripcion = des,
    Color = color2
  )
}
