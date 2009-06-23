\name{munsell}
\alias{munsell}
\title{Converts a Munsell colour to hex...}
\usage{munsell(hue, value, chroma, ...)}
\description{Converts a Munsell colour to hex}
\details{Takes a specifications of hue, value and chroma and returns the 
hex specification of that colour

Munsell colours are specified by hue, value and chroma.  They 
take a form like "5PB 5/10" where the first characters represent the
hue, followed by a space then the value and chroma separated by a "/". In
this package value should be an integer in 0:10 and chroma an even number
at most 24.  Note that not all possible specifications result in 
representable colours.  Regular recycling rules apply.}
\value{a character string specification of a hex colour}
\seealso{\code{\link{check.munsell}}, \code{\link{text.munsell}}}
\arguments{\item{hue}{a character vector of Munsell hues}
\item{value}{a numeric vector of values}
\item{chroma}{a numeric vector of chromas}
\item{...}{passed on to \code{\link{check.munsell}}. Use \code{fix = TRUE} to fix "bad" colours}}
\examples{munsell("5PB", 5, 10)
# All values of 5PB with chroma 10
munsell("5PB", 1:9, 10) # note some are undefined
# using in a ggplot plot
ggplot(data.frame(x = 1:9)) + geom_point(aes(x = x, y = x, colour = munsell("5PB", 1:9, 2))) + scale_colour_identity()
plot.hex(munsell("5PB", 1:9, 2))}
