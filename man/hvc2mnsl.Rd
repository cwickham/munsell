\name{hvc2mnsl}
\alias{hvc2mnsl}
\title{Converts a hue, chroma and value to a Munsell colour...}
\usage{hvc2mnsl(hue, value, chroma, ...)}

\description{
  Converts a hue, chroma and value to a Munsell colour
}

\details{
  Takes separate specifications of hue, value and chroma
  and returns the text specification of that colour.

  Munsell colours are specified by hue, value and chroma.
  They take a form like "5PB 5/10" where the first
  characters represent the hue, followed by a space then
  the value and chroma separated by a "/". In this package
  value should be an integer in 0:10 and chroma an even
  number at most 24.  Note that not all possible
  specifications result in representable colours.  Regular
  recycling rules apply.
}
\value{a character string specification of a hex colour}
\seealso{\code{\link{check_mnsl}}, \code{\link{mnsl2hex}}}
\arguments{
  \item{hue}{a character vector of Munsell hues}
  \item{value}{a numeric vector of values}
  \item{chroma}{a numeric vector of chromas}
  \item{...}{passed on to \code{\link{check_mnsl}}. Use \code{fix = TRUE} to
fix "bad" colours}
}
\examples{hvc2mnsl("5PB", 5, 10)
# All values of 5PB with chroma 10
hvc2mnsl("5PB", 1:9, 10) # note some are undefined
plot_mnsl(hvc2mnsl("5PB", 1:9, 2))}
