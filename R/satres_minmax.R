#' Get minmax
#'
#' Get all minmax values in vector form to check them.
#'
#' @param sr A `satres` object.
#'
#' @return A string vector.
#'
#' @export
satres_minmax <- function(sr) {
  res <- NULL
  for (n in names(sr$bands)) {
    res <-
      c(res, n, as.vector(round(terra::minmax(sr$bands[[n]], compute = FALSE), 2)))
  }
  res
}
