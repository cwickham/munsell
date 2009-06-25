\name{darker}
\alias{darker}
\title{Make a munsell colour darker...}
\usage{darker(col)}
\description{Make a munsell colour darker}
\details{Decreases the value of the Munsell colour by 1.}
\value{character vector of Munsell colours}
\arguments{\item{col}{character vector of Munsell colours}}
\examples{darker("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, darker(cols)))}
