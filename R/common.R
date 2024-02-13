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
    output <- paste0(tools::file_path_sans_ext(input), '.png')
  } else {
    output <- paste0(tools::file_path_sans_ext(output), '.png')
  }
  magick::image_write(img, path = output, format = "png")
  output
}


# adapt_image_folder ----------------------------------------------------

#' Fits an image to the given size.
#'
#' @param input A string, image folder.
#' @param output A string, image folder.
#' @param width A integer, width of the image.
#' @param height A integer, height of the image.
#'
#' @return A string, output image folder.
#'
#' @family common functions
#'
#' @examples
#' #
#'
#' @export
adapt_image_folder <- function(input, output = NULL, width = 800, height = 600) {
  if (is.null(output)) {
    output <- input
  }
  files <- list.files(input, pattern = '*.png|*.PNG|*.tif|*.TIF|*.jpg|*.JPG|*.jpeg|*.JPEG', full.names = TRUE)
  for (f in files) {
    adapt_image(input = f, output = paste0(output,'/', basename(f)), width = width, height = height)
  }
  output
}
