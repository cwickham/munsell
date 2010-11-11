\name{munsell.map}
\alias{munsell.map}
\docType{data}
\title{Provides link between Munsell colours hex and LUV}
\description{
Lookup table for converting Munsell colours to hex and LUV.
}
\usage{munsell.map}
\format{A dataframe containing columns hue, chroma, value, hex, name, L, U, V for 2740 Munsell colours.  Conversion from Munsell Renotation Data is done using formulae at \url{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html} and the function \code{hex} in the package colorspace.  Code for conversion can be found in the package directory in inst/raw/getmunsellmap.r}
\source{Munsell Renotation Data, RIT Munsell Color Science Laboratory.  \url{http://www.cis.rit.edu/mcsl/online/munsell.php}}
\keyword{datasets}
