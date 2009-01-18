library(ggplot2)
library(colorspace)
load("munsell_map.rdata")

#### munsell are viewed under illuminant C
### rgb in R is under illuminant D65??


#take standard munsell color i.e. "5PB 5/10"
munsell.text <- function(colour.spec){
  positions <- match(colour.spec, munsell.map$name)
  hex.vals <- munsell.map[positions, "hex"]
  if (any(is.na(hex.vals))) warning("Some colours were mispecified or 
    not available (in RGB) and assigned NA")
  return(hex.vals)
}


# takes hue (character), value(numeric) and chroma(numeric)
# outputs hex
munsell <- function(hue, value, chroma){
  selected <- paste(hue, " ", value, "/", chroma,  sep = "")
  hex.vals <- munsell.text(selected)
  return(hex.vals)
}

.plot_common <- function(bg.col){
  list(scale_fill_identity(), 
  opts(panel.grid.major = theme_blank(), 
  panel.grid.minor = theme_blank(), 
  panel.background = theme_blank(), 
  plot.background = theme_rect(fill = bg.col), 
  axis.line = theme_blank(), 
  axis.text.x = theme_blank(), 
  axis.text.y = theme_blank(),
  axis.ticks = theme_blank(),
  axis.ticks.y = theme_blank(),
  axis.title.x = theme_blank(),
  axis.title.y = theme_blank(),
  legend.background = theme_blank(),
  legend.key = theme_blank(),
  legend.text = theme_blank(),
  legend.title = theme_blank(), 
  drop = "legend_box"))
}

.plot_polar <- function(bg.col){
  list(scale_fill_identity(), 
  opts(panel.grid.major = theme_blank(), 
  panel.grid.minor = theme_blank(), 
  panel.background = theme_blank(), 
  plot.background = theme_rect(fill = bg.col), 
  axis.title.x = theme_blank(),
  legend.background = theme_blank(),
  legend.key = theme_blank(),
  legend.text = theme_blank(),
  legend.title = theme_blank(), 
  drop = "legend_box"))
}



hue.slice <- function(hue.name = "all",  back.col = "white"){
  if (hue.name == "all") {
    return(ggplot(aes(x = factor(chroma), y = factor(value)), 
      data = munsell.map) +
       geom_tile(aes(fill = hex), colour = back.col) +
      facet_wrap(~ hue) +
      scale_colour_manual(values = c("white","black")) +
      scale_x_discrete("Chroma") + 
      opts(aspect.ratio = 1) +
      scale_y_discrete("Value", expand = c(0.25, 0)) +
      .plot_common(back.col))
  }
  else {
    if (!hue.name %in% munsell.map$hue) stop("invalid hue name")
  ggplot(aes(x = factor(chroma), y = factor(value)), 
    data = subset(munsell.map, hue == hue.name)) +
     geom_tile(aes(fill = hex), colour = back.col, size = 1) +
    geom_text(aes(label = name, colour = value > 4), 
      angle = 45, size = 2) +
     scale_colour_manual(values = c("white","black")) +
    scale_x_discrete("Chroma") + 
    scale_y_discrete("Value", expand = c(0.25, 0)) +
    .plot_common(back.col) +
    opts(aspect.ratio = 1) 
  }
}

value.slice <- function(value.name,  back.col = "white"){
  if (!value.name %in% munsell.map$value) stop("invalid Value")
  ggplot(aes(x = hue, y = factor(chroma)), 
    data = subset(munsell.map, value == value.name & hue != "N")) +
     geom_tile(aes(fill = hex), colour = back.col) +
     coord_polar() +
    scale_x_discrete("Hue") + 
    scale_y_discrete("Chroma") +
    .plot_polar(back.col)
}

chroma.slice <- function(chroma.name,  back.col = "white"){
  if (!chroma.name %in% munsell.map$chroma) stop("invalid Chroma")
  ggplot(aes(x = hue, y = value), 
    data = subset(munsell.map, chroma == chroma.name & hue != "N")) +
     geom_tile(aes(fill = hex), colour = back.col) +
    geom_text(aes(label = name, colour = value > 4), 
      angle = 45, size = 2) +
     scale_colour_manual(values = c("white","black")) +
    scale_x_discrete("Hue") + 
    scale_y_continuous("Value") +
    opts(aspect.ratio = 1/4) +
    .plot_common(back.col)  
}

complement.slice <- function(hue.name,  back.col = "white"){
  if (!hue.name %in% munsell.map$hue) stop("invalid hue name")
  hues <- levels(munsell.map$hue)[-1]
  index <- which(hues == hue.name)
  complement <- hues[(index + 20) %% 40]
  munsell.sub <- subset(munsell.map, 
    hue == "N" | hue == hue.name | hue == complement)
  munsell.sub <- within(munsell.sub, {
    chroma[hue == complement] <- -chroma[hue == complement]
    hue <- factor(hue, levels = c(complement, "N", hues[index]))
    })
  
  ggplot(aes(x = factor(chroma), y = value), 
    data = munsell.sub) + 
     geom_tile(aes(fill = hex), colour = back.col,  size = 1) +
    geom_text(aes(label = name, colour = value > 4), 
      angle = 45, size = 2) +
     scale_colour_manual(values = c("white","black")) +
    scale_x_discrete("Chroma") + 
    scale_y_continuous("Value") +
    facet_grid(. ~ hue,  scales = "free_x",  space = "free")  +
    opts(aspect.ratio = 1) +
    .plot_common(back.col)
}

# converting rgb to munsell
# transform to uniform color space then find euclidean distance???
# LUV - uniform space

# convert hex/RGB to LUV
# compare euclidean distance to munsell colours also in LUV
# pick closest
# output hex/RGB and Munsell colour

rgb2munsell <- function(R, G = NULL, B = NULL){
    LUV.vals <- as(RGB(R, G, B), "LUV")@coords
    ncolors <- nrow(LUV.vals)
    dist.calc <- function(x, y) rowSums((rep(x, each = ncolors) - y) ^ 2)
    dists <- apply(munsell.map[, c("L", "U", "V")], 1, dist.calc, y = LUV.vals)
    if(is.null(dim(dists)))  closest <- which.min(dists)
    else closest <- apply(dists, 1, which.min)
    return(munsell.map[closest, "name"])
}

#plot rgb and closest
plot.closest <- function(R, G = NULL, B = NULL,  back.col = "white"){
  closest <- rgb2munsell(R, G, B)
  ncolours <- length(closest)
  rgbnames <- apply(round(RGB(R, G, B)@coords, 2), 1, paste, collapse = ", ")
  little.df <- data.frame(type = rep(c("actual", "closest"), each = ncolours),  
    hex = c(hex(RGB(R,G,B)),  munsell.text(closest)), 
    name = c(rgbnames, closest), 
    x = rep(c(0, 0), each = ncolours), y = rep(1:ncolours), 2)
  ggplot(data = little.df, aes(x = x, y = y)) + geom_tile(aes(fill = hex),
    colour = back.col, size = 2) +
    geom_text(aes(label = name), size = 2) +
    opts(aspect.ratio = ncolours) +
    .plot_common(back.col) + facet_wrap(~ type)
}


