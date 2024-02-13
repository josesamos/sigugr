# adapt_image ----------------------------------------------------

#' Fits an image to the given size.
#'
#' @param input A string, image file.
#' @param output A string, image file.
#' @param width A integer, width of the image.
#' @param height A integer, height of the image.
#'
#' @return A string, new image file.
#'
#' @family common functions
#'
#' @examples
#' #
#'
#' @export
adapt_image <- function(input, output = NULL, width = 800, height = 600) {
  fig <- magick::image_blank(width = width, height = height, color = "white")
  img <- magick::image_read(input)
  img <- magick::image_trim(img)
  img <- magick::image_scale(img, sprintf("%dx%d", width - 50, height - 50))
  img <- magick::image_composite(fig, img, gravity = "Center")
  if (is.null(output)) {
    output <- tempfile(pattern = tools::file_path_sans_ext(basename(input)), fileext = ".png")
  } else {
    output <- paste0(tools::file_path_sans_ext(output), '.png')
  }
  magick::image_write(img, path = output, format = "png")
  output
}
