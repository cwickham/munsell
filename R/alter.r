#' Make a munsell colour lighter
#'
#' Increases the value of the Munsell colour. 
#' @param col character vector of Munsell colours
#' @param steps number of steps to take in increasing value
#' @return character vector of Munsell colours
#' @export
#' @importFrom stats na.exclude
#' @examples 
#' lighter("5PB 2/4")
#' cols <- c("5PB 2/4", "5Y 6/8")
#' p <- plot_mnsl(c(cols, lighter(cols), lighter(cols, 2)))
#' p + ggplot2::facet_wrap(~ names, ncol = 2)
#' # lighter and darker are usually reversible
#' lighter(darker("5PB 2/4"))
#' # unless you try to pass though white or black
#' lighter(darker("5PB 1/4"))
lighter <- function(col, steps = 1){
  col <- na.exclude(col)
  col_hvc <- mnsl2hvc(as.vector(col))
  
  col_hvc[, "value"] <- col_hvc[, "value"] + steps
  # check edge cases
  whites <- col_hvc[, "value"] >= 10
  blacks <- col_hvc[, "value"] <= 0
  if (any(whites | blacks)){
    col_hvc[whites, "hue"] <- "N"
    col_hvc[whites, "value"] <- 10
    col_hvc[whites, "chroma"] <- 0
    col_hvc[blacks, "hue"] <- "N"
    col_hvc[blacks, "value"] <- 0
    col_hvc[blacks, "chroma"] <- 0
  }
  na_handle(col, hvc2mnsl(col_hvc))
}

na_handle <- function(naobj, res){
  nas <- attr(naobj, "na.action")
  if(is.null(nas)) return(res)
  
  if (is.vector(res) & is.vector(as.vector(naobj))){
    keep <- rep(NA, length(naobj) + length(nas))
    stopifnot(length(naobj) == length(res))
    keep[-nas] <- 1:length(res)
    result <- res[keep] 
  } else if(is.data.frame(res) & is.vector(as.vector(naobj))){
    keep <- rep(NA, length(naobj) + length(nas))
    stopifnot(length(naobj) == nrow(res))
    keep[-nas] <- 1:nrow(res)
    result <- res[keep, ]
  } else if(is.vector(res) & is.data.frame(naobj)){
    keep <- rep(NA, nrow(naobj) + length(nas))
    stopifnot(nrow(naobj) == length(res))
    keep[-nas] <- 1:length(res)
    result <- res[keep]
  } else if (is.data.frame(res) & is.data.frame(naobj)){
    keep <- rep(NA, nrow(naobj) + length(nas))
    stopifnot(nrow(naobj) == nrow(res))
    keep[-nas] <- 1:nrow(res)
    result <- res[keep, ]
  }
  
  result
}

#' Make a munsell colour darker
#'
#' Decreases the value of the Munsell colour by 1.
#' @param col character vector of Munsell colours
#' @param steps number of steps to take in decreasing value
#' @return character vector of Munsell colours
#' @export
#' @examples 
#' darker("5PB 3/4")
#' cols <- c("5PB 3/4", "5Y 7/8")
#' p <- plot_mnsl(c(cols, darker(cols), darker(cols, 2)))
#' p + ggplot2::facet_wrap(~ names, ncol = 2)
darker <- function(col, steps = 1){
  lighter(col, steps = -steps)
}

#' Make a munsell colour more saturated
#'
#' Increases the chroma of the Munsell colour by step steps (multiples of 2).
#' @param col character vector of Munsell colours
#' @param steps number of steps to take in increasing chroma
#' @return character vector of Munsell colours
#' @export
#' @importFrom stats na.exclude
#' @examples
#' saturate("5PB 2/4")
#' cols <- c("5PB 2/2", "5Y 7/6")
#' p <- plot_mnsl(c(cols, saturate(cols), saturate(cols, 2)))
#' p + ggplot2::facet_wrap(~ names, ncol = 2)
saturate <- function(col, steps = 1){
  col <- na.exclude(col)
  
  col_hvc <- mnsl2hvc(as.vector(col))
  col_hvc[, "chroma"] <- col_hvc[, "chroma"] + 2*steps
  greys <- col_hvc[, "chroma"] <= 0
  if (any(greys)){
    col_hvc[greys, "hue"] <- "N"
    col_hvc[greys, "chroma"] <- 0
  }  
  na_handle(col, hvc2mnsl(col_hvc))
}

#' Make a munsell colour less saturated
#'
#' Decreases the chroma of the Munsell colour by one step steps (multiples of 2).
#' @param col character vector of Munsell colours
#' @param steps number of steps to take in decreasing chroma
#' @return character vector of Munsell colours
#' @export
#' @examples 
#' desaturate("5PB 2/4")
#' cols <- c("5PB 2/6", "5Y 7/8")
#' p <- plot_mnsl(c(cols, desaturate(cols), desaturate(cols, 2)))
#' p + ggplot2::facet_wrap(~ names, ncol = 2)
desaturate <- function(col, steps = 1){
  saturate(col, steps = -steps)
}

#' Find the complement of a munsell colour 
#'
#' Finds the munsell colour with the same chroma and value but on the opposite
#' side of the hue circle. The complement is not defined 
#' for greys (hue == "N"), and the function returns the grey untransformed.
#' @param col character vector of Munsell colours
#' @param ... passed on to \code{\link{in_gamut}}. Use \code{fix = TRUE} to
#' fix "bad" complement
#' @return character vector of Munsell colours
#' @export
#' @importFrom stats na.exclude
#' @examples 
#' complement("5PB 2/4")
#' cols <- c("5PB 2/4", "5Y 7/8")
#' plot_mnsl(c(cols, complement(cols)))
complement <- function(col, ...){
  col <- na.exclude(col)
  
  col_hvc <- mnsl2hvc(as.vector(col))
  greys <- col_hvc[, "hue"] == "N"
  inds <- match(col_hvc$hue, mnsl_hues())
  col_hvc[, "hue"] <-  mnsl_hues()[((inds + 20 -1) %% 40) + 1]
  if (any(greys)){
    warning("Complement not defined for greys")
    col_hvc[greys, "hue"] <- "N"
  }
  na_handle(col, hvc2mnsl(col_hvc))
}

#' Generate a sequence of Munsell colours
#'
#' Generates a sequence of Munsell colours.  The sequence is generated by 
#' finding the closest munsell colours to a equidistant sequence of colours in  #' LUV space.
#' @param from character string of first Munsell colour
#' @param to character string of last Munsell colour
#' @param n number of colours in sequence
#' @param fix Should colours outside of the gamut be fixed? 
#' Passed on to \code{\link{fix_mnsl}}
#' @return character vector of Munsell colours
#' @export
#' @importFrom methods as
#' @examples
#' seq_mnsl("5R 2/4", "5R 5/16", 4)
#' plot_mnsl(seq_mnsl("5R 2/4", "5R 5/16", 4))
#' plot_mnsl(seq_mnsl("5R 5/6", 
#'   complement("5R 5/6"), 5))
seq_mnsl <- function(from, to, n, fix = FALSE){
  cols <- in_gamut(c(from, to), fix = fix)
  if(any(is.na(cols))) stop("Colors must be in gamut")
  in.LUV <- munsell.map[match(cols, munsell.map$name), c("L", "U", "V")]
  LUV.seq <- matrix(c(seq(in.LUV$L[1], in.LUV$L[2],  length = n), 
    seq(in.LUV$U[1], in.LUV$U[2],  length = n), 
    seq(in.LUV$V[1], in.LUV$V[2],  length = n)),  ncol = 3)
  rgb2mnsl(as(LUV(LUV.seq), "sRGB")@coords)
}



#' Change the hue of a munsell colour
#'
#' Moves the hue of a munsell colour in the direction red->yellow->green->blue->purple->red
#' @param col character vector of Munsell colours
#' @param steps number of hue steps to take
#' @return character vector of Munsell colours
#' @export
#' @importFrom stats na.exclude
#' @examples 
#' my_red <- "10R 4/8"
#' rygbp(my_red)
#' plot_mnsl(c(my_red, rygbp(my_red, 2), rygbp(my_red, 4)))
rygbp <- function(col, steps = 1){
  col <- na.exclude(col)
  
  col_hvc <- mnsl2hvc(as.vector(col))
  greys <- col_hvc[, "hue"] == "N"

  inds <- match(col_hvc$hue, mnsl_hues())
  col_hvc[, "hue"] <-  mnsl_hues()[((inds + steps -1) %% 40) + 1]
  if (any(greys)){
    warning("Greys returned untransformed")
    col_hvc[greys, "hue"] <- "N"
  }
  
  na_handle(col, hvc2mnsl(col_hvc))
}

#' Change the hue of a munsell colour
#'
#' Moves the hue of a munsell colour in the direction purple->blue->green->yellow->red->purple
#' @param col character vector of Munsell colours
#' @param steps number of hue steps to take
#' @return character vector of Munsell colours
#' @export
#' @examples 
#' my_red <- "2.5R 4/8"
#' pbgyr(my_red)
#' plot_mnsl(c(my_red, pbgyr(my_red, 2), pbgyr(my_red, 4)))
pbgyr <- function(col, steps = 1){
  rygbp(col, steps = -steps)
}
