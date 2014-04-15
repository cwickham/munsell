\name{pbgyr}
\alias{pbgyr}
\title{Change the hue of a munsell colour}
\usage{
pbgyr(col, steps = 1)
}
\arguments{
  \item{col}{character vector of Munsell colours}

  \item{steps}{number of hue steps to take}
}
\value{
character vector of Munsell colours
}
\description{
Moves the hue of a munsell colour in the direction
purple->blue->green->yellow->red->purple
}
\examples{
my_red <- "2.5R 4/8"
pbgyr(my_red)
plot_mnsl(c(my_red, pbgyr(my_red, 2), pbgyr(my_red, 4)))
}

