#' Default munsell plot theme
#' 
#' Removes unnecessary clutter in plots
#' @keywords internal
#' @param bg.col takes colour to use as background colour
theme_munsell <- function(bg.col = "white") {
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = NA),
      panel.grid.minor = ggplot2::element_line(colour = NA),
      panel.background = ggplot2::element_rect(fill = bg.col), 
      plot.background = ggplot2::element_blank(), 
      axis.line = ggplot2::element_line(colour = NA), 
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(),
      legend.title = ggplot2::element_text())
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE)
  }
  if(length(hex.colour) == 1) {
    add.ops <- list(ggplot2::geom_text(ggplot2::aes(label = names)))
  }
  else add.ops <- list(ggplot2::facet_wrap(~ names))
  
  df <- data.frame(colour = hex.colour, 
                   names = factor(hex.colour, levels=hex.colour), 
                   x = 0, y = 0)
  ggplot2::ggplot(data = df,  ggplot2::aes(x = x,  y = y)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = colour)) + 
    ggplot2::scale_fill_identity() + add.ops + 
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 1) + theme_munsell(back.col)    
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
#' p + ggplot2::facet_wrap(~ num, nrow = 1)
plot_mnsl <- function(cols,  back.col = "white", ...){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE)
  }
  add.ops <- NULL
  if(length(cols) > 1) {
     add.ops <- list(ggplot2::facet_wrap(~ num))
  }
  cols <- check_mnsl(cols)
  cols <- in_gamut(cols, ...)
  df <- data.frame(num = 1:length(cols), 
    names = factor(cols,  levels = c(unique(cols))),
    hex = mnsl2hex(cols), x = 0 , y = 0, stringsAsFactors = FALSE)
  df$labels <- factor(df$names,  levels = c(unique(cols), "NA"))
  df$labels[is.na(df$labels)] <- "NA"
  ggplot2::ggplot(data = df,  ggplot2::aes(x = x,  y = y)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = hex)) + 
    add.ops +
    ggplot2::geom_text(ggplot2::aes(label = labels, 
      colour = text_colour(as.character(names)))) +
    ggplot2::scale_x_continuous(expand = c(0, 0))+
    ggplot2::scale_y_continuous(expand = c(0, 0))+
    ggplot2::coord_fixed() +  
    theme_munsell(back.col) +
    ggplot2::scale_fill_identity() + 
    ggplot2::scale_colour_identity() + 
    ggplot2::theme(strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank())
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE)
  }
  if (any(hue.name == "all")) {
    return(
      ggplot2::ggplot(ggplot2::aes(x = factor(chroma), y = factor(value)),
        data = munsell.map) +
      ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col) +
      ggplot2::facet_wrap(~ hue) +
      ggplot2::scale_x_discrete("Chroma", expand = c(0, 0)) + 
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::scale_y_discrete("Value", expand = c(0, 0)) +
      theme_munsell(back.col) +
      ggplot2::scale_fill_identity()
      )
  }
  else {
    if (!all(hue.name %in% munsell.map$hue)) stop("invalid hue names")
    ggplot2::ggplot(ggplot2::aes(x = factor(chroma), y = factor(value)), 
        data = subset(munsell.map, hue %in% hue.name)) +
      ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col, size = 1) +
      ggplot2::geom_text(ggplot2::aes(label = name, colour = text_colour(name)), 
        angle = 45, size = 2) +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_x_discrete("Chroma") + 
      ggplot2::scale_y_discrete("Value", expand = c(0.125, 0)) +
      theme_munsell(back.col) +
      ggplot2::scale_fill_identity() +
      ggplot2::facet_wrap(~ hue)
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE)
  }
  if (!all(value.name %in% munsell.map$value)) stop("invalid Value")
  ggplot2::ggplot(ggplot2::aes(x = hue, y = factor(chroma)), 
      data = subset(munsell.map, value %in% value.name & hue != "N" & !is.na(hex))) +
    ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col) +
    ggplot2::coord_polar() +
    ggplot2::scale_x_discrete("Hue") + 
    ggplot2::scale_y_discrete("Chroma") +
    ggplot2::facet_wrap(~ value) +
    theme_munsell(back.col) +
    ggplot2::scale_fill_identity()
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
#' p + ggplot2::theme(axis.text = ggplot2::element_text(), 
#'    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
#' # all values 
#' \dontrun{chroma_slice(seq(0, 38, by = 2))}
chroma_slice <- function(chroma.name = seq(0, 26, by = 2),  back.col = "white"){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE)
  }
  if (!all(chroma.name %in% munsell.map$chroma)) stop("invalid Chroma")
  ggplot2::ggplot(ggplot2::aes(x = hue, y = value), 
      data = subset(munsell.map, chroma %in% chroma.name & hue != "N")) +
    ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col) +
    ggplot2::geom_text(ggplot2::aes(label = name, colour = text_colour(name)), 
      angle = 45, size = 2) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete("Hue") + 
    ggplot2::scale_y_continuous("Value") +
    ggplot2::coord_fixed(ratio = 1/4) +
    ggplot2::facet_wrap(~ chroma) +
    theme_munsell(back.col) +
    ggplot2::scale_fill_identity() 
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
#' complement_slice("10G")
complement_slice <- function(hue.name,  back.col = "white"){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE)
  }
  
  if (length(hue.name) > 1) stop("complement_slice currently only takes one hue")
  if (!hue.name %in% munsell.map$hue) stop("invalid hue name")
  comp.hue <- mnsl2hvc(complement(hvc2mnsl(hue.name, 2, 2)))$hue 
  munsell.sub <- subset(munsell.map, 
     hue == hue.name | hue == comp.hue)
  
  munsell.sub <- within(munsell.sub, {
    chroma <- ifelse(hue == comp.hue, -1, 1) * chroma
    hue <- factor(hue, levels = c(comp.hue, "N", hue.name))
    })
  
  ggplot2::ggplot(ggplot2::aes(x = chroma, y = value), 
      data = munsell.sub) + 
    ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col,  size = 1) +
    ggplot2::geom_text(ggplot2::aes(label = name, colour = text_colour(name)), 
      angle = 45, size = 2) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous("Chroma") + 
    ggplot2::scale_y_continuous("Value") +
    ggplot2::facet_grid(. ~ hue,  scales = "free_x", space = "free")  +
    ggplot2::coord_fixed() +
    theme_munsell(back.col)
  }

# 
# slice <- function(hue = NULL, chroma = NULL, value = NULL) {
#   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#     stop("ggplot2 needed for this function to work. Please install it.",
#       call. = FALSE)
#   }
#   spec <- as.list(match.call())[-1]
#   cols <- merge(munsell:::munsell.map, expand.grid(spec))
#   vars <- names(spec)
#   varying <- c("hue", "chroma", "value")[!(c("hue", "chroma", "value") %in% vars)]
#   if (length(vars) == 1){
#     print(ggplot(cols, aes_string(varying[1], varying[2])) +
#       geom_tile(aes(fill = hex),  size = 1) +
#       geom_text(aes(label = name, colour = text_colour(name)), 
#         angle = 45, size = 2) +
#       scale_fill_identity() +
#       scale_colour_identity() +
#       coord_fixed() +
#       theme_munsell())
#   } else if (length(vars) == 2){
#     print(ggplot(cols, aes_string(varying[1], 1)) +
#       geom_tile(aes(fill = hex),  size = 1) +
#       geom_text(aes(label = name, colour = text_colour(name)), 
#         angle = 45, size = 2) +
#       scale_fill_identity() +
#       scale_colour_identity() +
#       coord_fixed() +
#       theme_munsell())
#   }
#   cols[order(cols$hue, cols$chroma, cols$value), "name"]
# }


#' Plot closest Munsell colour to an sRGB colour
#'
#' Take an sRGB colour and plots it along with the closest Munsell colour (using \code{\link{rgb2mnsl}} to find it)
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE)
  }
  closest <- rgb2mnsl(R, G, B)
  ncolours <- length(closest)
  rgbnames <- apply(round(sRGB(R, G, B)@coords, 2), 1, paste, collapse = ", ")
  little.df <- data.frame(type = rep(c("actual", "closest"), each = ncolours),  
    hex = c(hex(sRGB(R,G,B)),  mnsl2hex(closest)), 
    name = c(rgbnames, closest), 
    x = rep(c(0, 0), each = ncolours), y = rep(1:ncolours, 2), 
    text.colour = rep(text_colour(closest), 2))
  ggplot2::ggplot(data = little.df, ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = hex),
      colour = back.col, size = 2) +
    ggplot2::geom_text(ggplot2::aes(label = name, colour = text.colour), size = 2) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_fixed(ratio = 1) +
    theme_munsell(back.col) +
    ggplot2::scale_fill_identity()+
    ggplot2::facet_wrap(~ type)
}

#' Get text colour
#'
#' Get the appropriate text colour for writing on a munsell colour.
#' @param a character vector of munsell colours
#' @return a vector of "black" or "white"
#' @export
#' @keywords internal
text_colour <- function(cols){
  values <-  mnsl2hvc(cols)[, "value"]
  ifelse(values >4, "black", "white")
}