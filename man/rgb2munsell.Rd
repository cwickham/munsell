\name{rgb2munsell}
\alias{rgb2munsell}
\title{Converts an RGB colour to Munsell...}
\usage{rgb2munsell(R, G, B)}
\description{Converts an RGB colour to Munsell}
\details{Finds the closest Munsell colour (in LUV space) to the specified RGB colour}
\seealso{\code{\link{plot.closest}}}
\arguments{\item{R,}{B, G take vectors of red, green and blue proportions or give R a 
3 column matrix with the proportions in the columns.}}
\examples{rgb2munsell(0.1, 0.1, 0.3)
rgb2munsell(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))
plot.closest(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))}
