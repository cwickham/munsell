\name{hue.slice}
\alias{hue.slice}
\title{Plot all colours with the same hue...}
\usage{hue.slice(hue.name=all, back.col=white)}
\description{Plot all colours with the same hue}
\details{Plots a slices of the Munsell colour system where hue is constant.}
\value{ggplot object}
\arguments{\item{hue.name}{character vector of the desired hues. Or "all" for all hues.}
\item{back.col}{colour for the background}}
\examples{hue.slice("5R")
hue.slice(c("5R", "5P"))
\dontrun{hue.slice("all")}}
