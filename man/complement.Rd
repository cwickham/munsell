\name{complement}
\alias{complement}
\title{Find the complement of a munsell colour...}
\usage{complement(col, ...)}
\description{Find the complement of a munsell colour}
\details{Finds the munsell colour with the same chroma and value but on the opposite
side of the hue circle.}
\value{character vector of Munsell colours}
\arguments{\item{col}{character vector of Munsell colours}
\item{...}{passed on to \code{\link{in_gamut}}. Use \code{fix = TRUE} to
fix "bad" complement}}
\examples{complement("5PB 2/4")
cols <- c("5PB 2/4", "5Y 7/8")
plot_mnsl(c(cols, complement(cols)))}
