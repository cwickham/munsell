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
#' @param colour.spec a character string representing a Munsell colour.
#' @param ... passed on to check.munsell. Use fix = TRUE to fix "bad" colours
#' @return a character string specification of a hex colour
#' @seealso check.munsell, munsell
#' @examples
#' munsell.text("5PB 5/10")
#' # use a munsell colour in a plot
#' ggplot(data.frame(x = 1:10)) + geom_point(aes(x = x, y = x), 
#'   colour = munsell.text("5PB 5/10"))
munsell.text <- function(colour.spec, ...){
  colour.spec <- check.munsell(colour.spec, ...)
  positions <- match(colour.spec, munsell.map$name)
  hex.vals <- munsell.map[positions, "hex"]
  return(hex.vals)
}

#' Converts a Munsell colour to hex
#'
#' Takes a specifications of hue, value and chroma and returns the 
#' hex specification of that colour
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
#' @param ... passed on to check.munsell. Use fix = TRUE to fix "bad" colours
#' @return a character string specification of a hex colour
#' @seealso check.munsell, text.munsell
#' @examples
#' munsell("5PB", 5, 10)
#' # All values of 5PB with chroma 10
#' munsell("5PB", 1:9, 10) # note some are undefined
#' # using in a ggplot plot
#' ggplot(data.frame(x = 1:9)) + geom_point(aes(x = x, y = x, colour = munsell("5PB", 1:9, 2))) + scale_colour_identity()
#' plot.hex(munsell("5PB", 1:9, 2))
munsell <- function(hue, value, chroma, ...){
  selected <- paste(hue, " ", value, "/", chroma,  sep = "")
  hex.vals <- munsell.text(selected, ...)
  return(hex.vals)
}

#' Converts an RGB colour to Munsell
#'
#' Finds the closest Munsell colour (in LUV space) to the specified RGB colour
#'
#' @param R, B, G take vectors of red, green and blue proportions or give R a 
#' 3 column matrix with the proportions in the columns.
#' @seealso plot.closest
#' @examples
#' rgb2munsell(0.1, 0.1, 0.3)
#' rgb2munsell(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))
#' plot.closest(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3))
rgb2munsell <- function(R, G = NULL, B = NULL){
    LUV.vals <- as(RGB(R, G, B), "LUV")@coords
    ncolors <- nrow(LUV.vals)
    dist.calc <- function(x, y) rowSums((rep(x, each = ncolors) - y) ^ 2)
    dists <- apply(munsell.map[, c("L", "U", "V")], 1, dist.calc, y = LUV.vals)
    if(is.null(dim(dists)))  closest <- which.min(dists)
    else closest <- apply(dists, 1, which.min)
    return(munsell.map[closest, "name"])
}
