\name{fix.munsell}
\alias{fix.munsell}
\title{Fix an undefined Munsell colour...}
\usage{fix.munsell(colour.spec)}
\description{Fix an undefined Munsell colour}
\details{Takes correctly specified but undefined colours and outputs something sensible.  Normally this happens when the chroma is too high.  So,  here sensible means the colour with the same hue and value and maximum defined chroma.}
\value{a character vector containing the fixed colours.}
\arguments{\item{colour.spec}{a character vector representing Munsell colours.}}
\examples{fix.munsell(c("5R 5/8","2.5R 9/28"))}
