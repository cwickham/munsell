\name{lighter}
\alias{lighter}
\title{Make a munsell colour lighter}
\usage{
  lighter(col)
}
\arguments{
  \item{col}{character vector of Munsell colours}
}
\value{
  character vector of Munsell colours
}
\description{
  Increases the value of the Munsell colour by 1.
}
\examples{
lighter("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, lighter(cols)))
}

