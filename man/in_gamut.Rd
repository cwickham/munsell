\name{in_gamut}
\alias{in_gamut}
\title{Checks if a Munsell colour is defined in RGB space...}
\usage{in_gamut(col, fix=FALSE)}
\description{Checks if a Munsell colour is defined in RGB space}
\details{Not all possible correctly formatted Munsell colours result in a colour
representable in RGB space.  This function checks if the colour is
representable.}
\value{a character vector containing the input colours.  If any colours
were outside the gamut they will be represented by NA.}
\keyword{internal}
\arguments{\item{col}{a character vector representing Munsell colours.}
\item{fix}{passed on to \code{\link{fix_mnsl}}. Use \code{fix = TRUE} to
fix "bad" colours}}
\examples{in_gamut(c("5R 5/8","2.5R 9/28"))}
