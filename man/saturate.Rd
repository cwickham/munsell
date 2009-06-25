\name{saturate}
\alias{saturate}
\title{Make a munsell colour more saturated...}
\usage{saturate(col)}
\description{Make a munsell colour more saturated}
\details{Increases the chroma of the Munsell colour by one step (+ 2).}
\value{character vector of Munsell colours}
\arguments{\item{col}{character vector of Munsell colours}}
\examples{saturate("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, saturate(cols)))}
