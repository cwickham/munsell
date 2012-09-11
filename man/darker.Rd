\name{darker}
\alias{darker}
\title{Make a munsell colour darker}
\usage{
  darker(col)
}
\arguments{
  \item{col}{character vector of Munsell colours}
}
\value{
  character vector of Munsell colours
}
\description{
  Decreases the value of the Munsell colour by 1.
}
\examples{
darker("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, darker(cols)))
}

