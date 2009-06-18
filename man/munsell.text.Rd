\name{munsell.text}
\alias{munsell.text}
\title{Converts a Munsell colour to hex...}
\usage{munsell.text(colour.spec, ...)}
\description{Converts a Munsell colour to hex}
\details{Take a character string representation of a Munsell colour and returns the 
hex specification of that colour

Munsell colours are specified by hue, value and chroma.  They 
take a form like "5PB 5/10" where the first characters represent the
hue, followed by a space then the value and chroma separated by a "/". In
this package value should be an integer in 0:10 and chroma an even number
at most 24.  Note that not all possible specifications result in 
representable colours.}
\value{a character string specification of a hex colour}
\seealso{\code{\link{check.munsell}}, \code{\link{munsell}}}
\arguments{\item{colour.spec}{a character string representing a Munsell colour.}
\item{...}{passed on to check.munsell. Use fix = TRUE to fix "bad" colours}}
\examples{munsell.text("5PB 5/10")
# use a munsell colour in a plot
ggplot(data.frame(x = 1:10)) + geom_point(aes(x = x, y = x), 
colour = munsell.text("5PB 5/10"))}
