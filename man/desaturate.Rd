\name{desaturate}
\alias{desaturate}
\title{Make a munsell colour less saturated...}
\usage{desaturate(col)}

\description{
  Make a munsell colour less saturated
}

\details{
  Decreases the chroma of the Munsell colour by one step (-
  2).
}
\value{character vector of Munsell colours}
\arguments{
  \item{col}{character vector of Munsell colours}
}
\examples{desaturate("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, desaturate(cols)))}
