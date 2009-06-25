\name{lighter}
\alias{lighter}
\title{Make a munsell colour lighter...}
\usage{lighter(col)}
\description{Make a munsell colour lighter}
\details{Increases the value of the Munsell colour by 1.}
\value{character vector of Munsell colours}
\arguments{\item{col}{character vector of Munsell colours}}
\examples{lighter("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, lighter(cols)))}
