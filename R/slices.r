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