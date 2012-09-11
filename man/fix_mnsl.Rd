\name{fix_mnsl}
\alias{fix_mnsl}
\title{Fix an undefined Munsell colour}
\usage{
  fix_mnsl(col)
}
\arguments{
  \item{col}{a character vector representing Munsell
  colours.}
}
\value{
  a character vector containing the fixed colours.
}
\description{
  Takes correctly specified but undefined colours and
  outputs something sensible.  Normally this happens when
  the chroma is too high.  So, here sensible means the
  colour with the same hue and value and maximum defined
  chroma.
}
\examples{
fix_mnsl(c("5R 5/8","2.5R 9/28"))
}
\keyword{internal}

