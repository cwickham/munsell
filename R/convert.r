#' Converts a Munsell colour to hex
#'
#' Take a character string representation of a Munsell colour and returns the 
#' hex specification of that colour
#'
#' Munsell colours are specified by hue, value and chroma.  They 
#' take a form like "5PB 5/10" where the first characters represent the
#' hue, followed by a space then the value and chroma separated by a "/". In
#' this package value should be an integer in 0:10 and chroma an even number
#' at most 24.  Note that not all possible specifications result in 
#' representable colours.  
#' @param col a character string representing a Munsell colour.
#' @param ... passed on to \code{\link{check_mnsl}}. Use \code{fix = TRUE} to
#' fix "bad" colours
#' @return a character string specification of a hex colour
#' @seealso \code{\link{check_mnsl}}, \code{\link{hvc2mnsl}}
#' @aliases mnsl2hex mnsl
#' @export mnsl2hex mnsl
#' @examples
#' mnsl2hex("5PB 5/10")
#' # use a munsell colour in a plot
#' require("ggplot2")
#' ggplot(data.frame(x = 1:10)) + geom_point(aes(x = x, y = x), 
#'   colour = mnsl2hex("5PB 5/10"))
mnsl <- function(col, ...){
  col <- check_mnsl(col, ...)
  positions <- match(col, munsell.map$name)
  munsell.map[positions, "hex"]
}
mnsl2hex <- mnsl

#' Converts a hue, chroma and value to a Munsell colour
#'
#' Takes separate specifications of hue, value and chroma and returns the 
#' text specification of that colour.
#'
#' Munsell colours are specified by hue, value and chroma.  They 
#' take a form like "5PB 5/10" where the first characters represent the
#' hue, followed by a space then the value and chroma separated by a "/". In
#' this package value should be an integer in 0:10 and chroma an even number
#' at most 24.  Note that not all possible specifications result in 
#' representable colours.  Regular recycling rules apply.
#' @param hue a character vector of Munsell hues
#' @param value a numeric vector of values
#' @param chroma a numeric vector of chromas
#' @param ... passed on to \code{\link{check_mnsl}}. Use \code{fix = TRUE} to
#' fix "bad" colours
#' @return a character string specification of a hex colour
#' @seealso \code{\link{check_mnsl}}, \code{\link{mnsl2hex}}
#' @export
#' @examples
#' hvc2mnsl("5PB", 5, 10)
#' # All values of 5PB with chroma 10
#' hvc2mnsl("5PB", 1:9, 10) # note some are undefined
#' plot_mnsl(hvc2mnsl("5PB", 1:9, 2))
hvc2mnsl <- function(hue, value, chroma, ...){
  selected <- paste(hue, " ", value, "/", chroma,  sep = "")
  selected <- check_mnsl(selected, ...)
  selected
}

#' Converts an RGB colour to Munsell
#'
#' Finds the closest Munsell colour (in LUV space) to the specified RGB colour
#'
#' @param R a numeric vector of red values or a 3 column matrix with the
#' proportions R,  G,  B in the columns.
#' @param G numeric vector of green values
#' @param B numeric vector of blue values
#' @seealso \code{\link{plot_closest}}
#' @export
#' @examples
#' rgb2mnsl(0.1, 0.1, 0.3)
#' rgb2mnsl(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))
#' plot_closest(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))
rgb2mnsl <- function(R, G = NULL, B = NULL){
    LUV.vals <- as(RGB(R, G, B), "LUV")@coords
    ncolors <- nrow(LUV.vals)
    dist.calc <- function(x, y) rowSums((rep(x, each = ncolors) - y) ^ 2)
    dists <- apply(munsell.map[, c("L", "U", "V")], 1, dist.calc, y = LUV.vals)
    if(is.null(dim(dists)))  closest <- which.min(dists)
    else closest <- apply(dists, 1, which.min)
    munsell.map[closest, "name"]
}
