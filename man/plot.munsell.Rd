\name{plot.munsell}
\alias{plot.munsell}
\title{Plot a munsell colour...}
\usage{plot.munsell(colour.specs, back.col=white, ...)}
\description{Plot a munsell colour}
\details{Takes munsell text specifications and plots colour squares of them.}
\value{A ggplot object}
\arguments{\item{colour.specs}{character vector specifying colours in Munsell form}
\item{back.col}{specification of background colour of display}
\item{...}{passed to check.munsell. Add fix = TRUE to fix "bad" colours()}}
\examples{plot.munsell("5R 5/6")
plot.munsell("5R 5/6",  back.col = "grey40")
p <- plot.munsell(c("5R 6/6", "5Y 6/6", "5G 6/6", "5B 6/6", "5P 6/6"),
back.col = "grey40")
p
# returned object is a ggplot object so we can alter the layout
summary(p)
p + facet_wrap(~ names, nrow = 1)}
