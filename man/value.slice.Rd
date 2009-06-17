\name{value.slice}
\alias{value.slice}
\title{Plot all colours with the same value...}
\usage{value.slice(value.name, back.col=white)}
\description{Plot all colours with the same value}
\details{Plots slices of the Munsell colour system where value is constant.}
\value{ggplot object}
\arguments{\item{value.name}{integer vector of the desired values.}
\item{back.col}{colour for the background}}
\examples{value.slice(2)
value.slice(c(2, 4))
# all values 
\dontrun{value.slice(1:10)}}
