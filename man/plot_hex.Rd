\name{plot_hex}
\alias{plot_hex}
\title{Plot hex colours...}
\usage{plot_hex(hex.colour, back.col="white")}

\description{
  Plot hex colours
}

\details{
  Quick way to look at a set of hex colours.
}
\value{A ggplot object}
\arguments{
  \item{hex.colour}{character vector specifying colours in hex form}
  \item{back.col}{specification of background colour of display}
}
\examples{plot_hex("#000000")
plot_hex(c("#000000","#FFFFFF"))}
