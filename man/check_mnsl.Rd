\name{check_mnsl}
\alias{check_mnsl}
\title{Checks for valid Munsell colours}
\usage{
  check_mnsl(col, fix = FALSE)
}
\arguments{
  \item{col}{a character vector representing Munsell
  colours.}

  \item{fix}{passed on to \code{\link{fix_mnsl}}. Use
  \code{fix = TRUE} to fix "bad" colours}
}
\value{
  a character vector containing the input colours.  If any
  colours were outside the gamut they will be represented
  by NA.
}
\description{
  Checks user supplied munsell specification for validity.
  I.e. colour is of form "h v/c" and h, v and c take valid
  values.
}
\examples{
check_mnsl(c("5R 5/8","2.5R 9/28"))
}
\keyword{internal}

