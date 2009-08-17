\name{plot_closest}
\alias{plot_closest}
\title{Plot closest Munsell colour to an RGB colour...}
\usage{plot_closest(R, G, B, back.col="white")}
\description{Plot closest Munsell colour to an RGB colour}
\details{Take an RGB colour and plots it along with the closest Munsell colour (using \code{\link{rgb2mnsl}} to find it)}
\seealso{\code{\link{rgb2mnsl}}}
\value{ggplot object}
\arguments{\item{R}{a numeric vector of red values or a 3 column matrix with the 
proportions R,  G,  B in the columns.}
\item{G}{numeric vector of green values}
\item{B}{numeric vector of blue values}
\item{back.col}{colour for the background}}
\examples{plot_closest(0.1, 0.1, 0.3)
plot_closest(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))}
