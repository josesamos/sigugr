% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clip_raster.R
\name{clip_raster}
\alias{clip_raster}
\title{Clip a raster layer based on a polygon}
\usage{
clip_raster(raster, polygon)
}
\arguments{
\item{raster}{A `terra` raster layer.}

\item{polygon}{A `sf` polygon layer.}
}
\value{
A `terra` raster layer.
}
\description{
The result preserves the CRS of the raster.
}
\examples{
#

}
\seealso{
Other transformation functions: 
\code{\link{clip_multipoligon}()},
\code{\link{clip_vector}()},
\code{\link{copy_styles_layer}()}
}
\concept{transformation functions}
