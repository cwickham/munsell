\name{plot.closest}
\alias{plot.closest}
\title{Plot closest Munsell colour to an RGB colour...}
\usage{plot.closest(R, G, B, back.col=white)}
\description{Plot closest Munsell colour to an RGB colour}
\details{Take an RGB colour and plots it along with the closest Munsell colour (using #' rgb2munsell to find it)}
\seealso{rgb2munsell}
\value{ggplot object}
\arguments{\item{R,}{B, G take vectors of red, green and blue proportions or give R a 
3 column matrix with the proportions in the columns.}
\item{back.col}{colour for the background}}
\examples{plot.closest(0.1, 0.1, 0.3)
plot.closest(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))}
