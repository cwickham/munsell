\name{value_slice}
\alias{value_slice}
\title{Plot all colours with the same value}
\usage{
  value_slice(value.name = 1:10, back.col = "white")
}
\arguments{
  \item{value.name}{integer vector of the desired values.}

  \item{back.col}{colour for the background}
}
\value{
  ggplot object
}
\description{
  Plots slices of the Munsell colour system where value is
  constant.
}
\examples{
value_slice(2)
value_slice(c(2, 4))
# all values
\dontrun{value_slice(1:10)}
}

