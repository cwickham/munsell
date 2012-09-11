\name{plot_mnsl}
\alias{plot_mnsl}
\title{Plot a munsell colour}
\usage{
  plot_mnsl(cols, back.col = "white", ...)
}
\arguments{
  \item{cols}{character vector specifying colours in
  Munsell form}

  \item{back.col}{specification of background colour of
  display}

  \item{...}{passed to \code{\link{check_mnsl}}. Add fix =
  TRUE to fix "bad" colours()}
}
\value{
  A ggplot object
}
\description{
  Takes munsell text specifications and plots colour
  squares of them.
}
\examples{
plot_mnsl("5R 5/6")
plot_mnsl("5R 5/6",  back.col = "grey40")
p <- plot_mnsl(c("5R 6/6", "5Y 6/6", "5G 6/6", "5B 6/6", "5P 6/6"),
 back.col = "grey40")
p
# returned object is a ggplot object so we can alter the layout
summary(p)
p + facet_wrap(~ names, nrow = 1)
}

