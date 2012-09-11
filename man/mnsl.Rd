\name{mnsl}
\alias{mnsl}
\alias{mnsl2hex}
\title{Converts a Munsell colour to hex}
\usage{
  mnsl(col, ...)
}
\arguments{
  \item{col}{a character string representing a Munsell
  colour.}

  \item{...}{passed on to \code{\link{check_mnsl}}. Use
  \code{fix = TRUE} to fix "bad" colours}
}
\value{
  a character string specification of a hex colour
}
\description{
  Take a character string representation of a Munsell
  colour and returns the hex specification of that colour
}
\details{
  Munsell colours are specified by hue, value and chroma.
  They take a form like "5PB 5/10" where the first
  characters represent the hue, followed by a space then
  the value and chroma separated by a "/". In this package
  value should be an integer in 0:10 and chroma an even
  number at most 24.  Note that not all possible
  specifications result in representable colours.
}
\examples{
mnsl2hex("5PB 5/10")
# use a munsell colour in a plot
require("ggplot2")
ggplot(data.frame(x = 1:10)) + geom_point(aes(x = x, y = x),
  colour = mnsl2hex("5PB 5/10"))
}
\seealso{
  \code{\link{check_mnsl}}, \code{\link{hvc2mnsl}}
}

