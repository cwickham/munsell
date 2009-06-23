\name{check.munsell}
\alias{check.munsell}
\title{Checks for valid Munsell colours...}
\usage{check.munsell(colour.spec, fix=FALSE)}
\description{Checks for valid Munsell colours}
\details{Checks user supplied munsell specification for validity.  
I.e. colour is of form "h v/c" and h,  v and c take valid values.}
\value{a character vector containing the input colours.  If any colours were outside the gamut they will be represented by NA.}
\arguments{\item{colour.spec}{a character vector representing Munsell colours.}
\item{fix}{passed on to \code{\link{fix.munsell}}. Use \code{fix = TRUE} to fix "bad" colours}}
\examples{check.munsell(c("5R 5/8","2.5R 9/28"))}
