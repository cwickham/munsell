\name{chroma.slice}
\alias{chroma.slice}
\title{Plot all colours with the same chroma...}
\usage{chroma.slice(chroma.name, back.col=white)}
\description{Plot all colours with the same chroma}
\details{Plots slices of the Munsell colour system where chroma is constant.}
\value{ggplot object}
\arguments{\item{chroma.name}{integer vector of the desired values.}
\item{back.col}{colour for the background}}
\examples{chroma.slice(2)
chroma.slice(18)
Maybe want to delete text and add axis instead
p <- chroma.slice(18)
p$layers[[2]] <- NULL # remove text layer
p + opts(axis.text.x = theme_text(angle = 90, hjust = 1), 
axis.text.y = theme_text())  
# all values 
\dontrun{chroma.slice(seq(0, 38, by = 2))}}
