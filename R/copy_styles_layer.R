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
  sf::st_write(obj = my_style, dsn = to, layer = layer, quiet = TRUE)
}
