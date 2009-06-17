\name{complement.slice}
\alias{complement.slice}
\title{A vertical slice through the Munsell space...}
\usage{complement.slice(hue.name, back.col=white)}
\description{A vertical slice through the Munsell space}
\details{Plot a hue and its complement at all values and chromas}
\value{ggplot object}
\arguments{\item{hue.name}{character string of the desired hue.}
\item{back.col}{colour for the background}}
\examples{complement.slice("5PB")
complement.slice("5R")}
