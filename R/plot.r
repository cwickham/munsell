#' Default munsell plot theme
#' 
#' Removes unnecessary clutter in plots
#' @keywords internal
#' @param bg.col takes colour to use as background colour
theme_munsell <- function(bg.col) {
    theme(
      panel.grid.major = element_line(colour = NA),
      panel.grid.minor = element_line(colour = NA),
      panel.background = element_rect(fill = bg.col), 
      plot.background = element_blank(), 
      axis.line = element_line(colour = NA), 
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(),
      legend.title = element_text())
}


#' Plot hex colours
#'
#' Quick way to look at a set of hex colours.
#' @param hex.colour character vector specifying colours in hex form
#' @param back.col specification of background colour of display
#' @return A ggplot object
#' @export
#' @examples
#' plot_hex("#000000")
#' plot_hex(c("#000000","#FFFFFF"))
plot_hex <- function(hex.colour,  back.col = "white"){
  require("ggplot2")
  if(length(hex.colour) == 1) add.ops <- list(geom_text(aes(label = names)))
  else add.ops <- list(facet_wrap(~ names))
  
  df <- data.frame(colour = hex.colour, 
                   names = factor(hex.colour, levels=hex.colour), 
                   x = 0, y = 0)
  ggplot(data = df,  aes(x = x,  y = y)) + geom_tile(aes(fill = colour)) + 
     scale_fill_identity() + add.ops + 
     scale_x_continuous(expand = c(0, 0))+
     scale_y_continuous(expand = c(0, 0))+
     coord_fixed(ratio = 1) + theme_munsell(back.col)    
}

#' Plot a munsell colour
#'
#' Takes munsell text specifications and plots colour squares of them.
#' @param cols character vector specifying colours in Munsell form
#' @param back.col specification of background colour of display
#' @param ... passed to \code{\link{check_mnsl}}. Add fix = TRUE to fix "bad" colours()
#' @return A ggplot object
#' @export
#' @examples
#' plot_mnsl("5R 5/6")
#' plot_mnsl("5R 5/6",  back.col = "grey40")
#' p <- plot_mnsl(c("5R 6/6", "5Y 6/6", "5G 6/6", "5B 6/6", "5P 6/6"),
#'  back.col = "grey40")
#' p
#' # returned object is a ggplot object so we can alter the layout
#' summary(p)
#' p + facet_wrap(~ names, nrow = 1)
plot_mnsl <- function(cols,  back.col = "white", ...){
  require("ggplot2")

  if(length(cols) == 1) {add.ops <- list(
    geom_text(aes(label = names, colour = text_colour(as.character(names)))), 
    scale_colour_identity())}
  else add.ops <- list(facet_wrap(~ names))
  cols <- check_mnsl(cols, ...)
  df <- data.frame(names = factor(cols, levels = cols),  
    hex = mnsl2hex(cols), x = 0 , y = 0)
  ggplot(data = df,  aes(x = x,  y = y)) + geom_tile(aes(fill = hex)) + 
    add.ops +
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0))+
    coord_fixed() +  
    theme_munsell(back.col) +
    scale_fill_identity()
}
    

#' Plot all colours with the same hue
#'
#' Plots slices of the Munsell colour system where hue is constant.
#' @param hue.name character vector of the desired hues. Or "all" for all hues.
#' @param back.col colour for the background
#' @return ggplot object
#' @export
#' @examples
#' hue_slice("5R")
#' hue_slice(c("5R", "5P"))
#' \dontrun{hue_slice("all")}
hue_slice <- function(hue.name = "all",  back.col = "white"){
  require("ggplot2")

  if (any(hue.name == "all")) {
    return(ggplot(aes(x = factor(chroma), y = factor(value)), 
      data = munsell.map) +
       geom_tile(aes(fill = hex), colour = back.col) +
      facet_wrap(~ hue) +
      scale_x_discrete("Chroma", expand = c(0, 0)) + 
      coord_fixed(ratio = 1) +
      scale_y_discrete("Value", expand = c(0, 0)) +
      theme_munsell(back.col) +
      scale_fill_identity())
  }
  else {
    if (!all(hue.name %in% munsell.map$hue)) stop("invalid hue names")
  ggplot(aes(x = factor(chroma), y = factor(value)), 
    data = subset(munsell.map, hue %in% hue.name)) +
     geom_tile(aes(fill = hex), colour = back.col, size = 1) +
     geom_text(aes(label = name, colour = text_colour(name)), 
      angle = 45, size = 2) +
     scale_colour_identity() +
    scale_x_discrete("Chroma") + 
    scale_y_discrete("Value", expand = c(0.125, 0)) +
    theme_munsell(back.col) +
    scale_fill_identity()+
    facet_wrap(~ hue)
  }
}

#' Plot all colours with the same value
#'
#' Plots slices of the Munsell colour system where value is constant.
#' @param value.name integer vector of the desired values. 
#' @param back.col colour for the background
#' @return ggplot object
#' @export
#' @examples
#' value_slice(2)
#' value_slice(c(2, 4))
#' # all values 
#' \dontrun{value_slice(1:10)}
value_slice <- function(value.name = 1:10,  back.col = "white"){
  require("ggplot2")

  if (!all(value.name %in% munsell.map$value)) stop("invalid Value")
  ggplot(aes(x = hue, y = factor(chroma)), 
    data = subset(munsell.map, value %in% value.name & hue != "N" & !is.na(hex))) +
     geom_tile(aes(fill = hex), colour = back.col) +
     coord_polar() +
    scale_x_discrete("Hue") + 
    scale_y_discrete("Chroma") +
    facet_wrap(~ value) +
    theme_munsell(back.col) +
    scale_fill_identity()
}

#' Plot all colours with the same chroma
#'
#' Plots slices of the Munsell colour system where chroma is constant.
#' @param chroma.name integer vector of the desired values. 
#' @param back.col colour for the background
#' @return ggplot object
#' @export
#' @examples
#' chroma_slice(2)
#' chroma_slice(18)
#' # Maybe want to delete text and add axis instead
#' p <- chroma_slice(18)
#' p$layers[[2]] <- NULL # remove text layer
#' p + theme(axis.text = element_text(), 
#'           axis.text.x = element_text(angle = 90, hjust = 1))
#' # all values 
#' \dontrun{chroma_slice(seq(0, 38, by = 2))}
chroma_slice <- function(chroma.name = seq(0, 26, by = 2),  back.col = "white"){
  require("ggplot2")

  if (!all(chroma.name %in% munsell.map$chroma)) stop("invalid Chroma")
  ggplot(aes(x = hue, y = value), 
    data = subset(munsell.map, chroma %in% chroma.name & hue != "N")) +
     geom_tile(aes(fill = hex), colour = back.col) +
    geom_text(aes(label = name, colour = text_colour(name)), 
      angle = 45, size = 2) +
     scale_colour_identity() +
    scale_x_discrete("Hue") + 
    scale_y_continuous("Value") +
    coord_fixed(ratio = 1/4) +
    facet_wrap(~ chroma) +
    theme_munsell(back.col) +
    scale_fill_identity() 
}

#' A vertical slice through the Munsell space
#'
#' Plot a hue and its complement at all values and chromas
#' @param hue.name character string of the desired hue. 
#' @param back.col colour for the background
#' @return ggplot object
#' @export
#' @examples
#' complement_slice("5PB")
#' complement_slice("5R")
complement_slice <- function(hue.name,  back.col = "white"){
  require("ggplot2")

  if (length(hue.name) > 1) stop("complement_slice currently only takes one hue")
  if (!hue.name %in% munsell.map$hue) stop("invalid hue name")
  hues <- levels(munsell.map$hue)[-1]
  index <- which(hues == hue.name)
  comp.hue <- hues[(index + 20) %% 40]
  munsell.sub <- subset(munsell.map, 
     hue == hue.name | hue == comp.hue)
  munsell.sub <- within(munsell.sub, {
    chroma <- ifelse(hue == comp.hue, -1, 1) * chroma
    hue <- factor(hue, levels = c(comp.hue, "N", hues[index]))
    })
  
  ggplot(aes(x = chroma, y = value), 
    data = munsell.sub) + 
     geom_tile(aes(fill = hex), colour = back.col,  size = 1) +
    geom_text(aes(label = name, colour = text_colour(name)), 
      angle = 45, size = 2) +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_x_continuous("Chroma") + 
    scale_y_continuous("Value") +
    facet_grid(. ~ hue,  scales = "free_x", space = "free")  +
    coord_fixed() +
    theme_munsell(back.col)
  }

#' Plot closest Munsell colour to an RGB colour
#'
#' Take an RGB colour and plots it along with the closest Munsell colour (using \code{\link{rgb2mnsl}} to find it)
#' @param R a numeric vector of red values or a 3 column matrix with the 
#' proportions R,  G,  B in the columns.
#' @param G numeric vector of green values
#' @param B numeric vector of blue values
#' @param back.col colour for the background
#' @seealso \code{\link{rgb2mnsl}}
#' @return ggplot object
#' @export
#' @examples
#' plot_closest(0.1, 0.1, 0.3)
#' plot_closest(matrix(c(.1, .2, .4, .5, .6, .8),  ncol = 3)) 
plot_closest <- function(R, G = NULL, B = NULL,  back.col = "white"){
  require("ggplot2")

  closest <- rgb2mnsl(R, G, B)
  ncolours <- length(closest)
  rgbnames <- apply(round(RGB(R, G, B)@coords, 2), 1, paste, collapse = ", ")
  little.df <- data.frame(type = rep(c("actual", "closest"), each = ncolours),  
    hex = c(hex(RGB(R,G,B)),  mnsl2hex(closest)), 
    name = c(rgbnames, closest), 
    x = rep(c(0, 0), each = ncolours), y = rep(1:ncolours, 2), 
    text.colour = rep(text_colour(closest), 2))
  ggplot(data = little.df, aes(x = x, y = y)) + geom_tile(aes(fill = hex),
    colour = back.col, size = 2) +
    geom_text(aes(label = name, colour = text.colour), size = 2) +
    scale_colour_identity() +
    coord_fixed(ratio = 1) +
    theme_munsell(back.col) +
    scale_fill_identity()+
    facet_wrap(~ type)
}

#' Get text colour
#'
#' Get the appropriate text colour for writing on a munsell colour.
#' @param a character vector of munsell colours
#' @return a vector of "black" or "white"
#' @export
#' @keywords internal
text_colour <- function(cols){
  col.split <- lapply(strsplit(cols, "/"), 
     function(x) unlist(strsplit(x, " ")))
  col.split <- lapply(col.split, gsub, pattern = "[A-Z]", replacement = "")
  values <- as.numeric(sapply(col.split, "[", 2))
  ifelse(values >4, "black", "white")
}