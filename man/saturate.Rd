\name{saturate}
\alias{saturate}
\title{Make a munsell colour more saturated}
\usage{
  saturate(col)
}
\arguments{
  \item{col}{character vector of Munsell colours}
}
\value{
  character vector of Munsell colours
}
\description{
  Increases the chroma of the Munsell colour by one step (+
  2).
}
\examples{
saturate("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, saturate(cols)))
}

