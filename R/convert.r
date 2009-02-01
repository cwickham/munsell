#take standard munsell color i.e. "5PB 5/10"
munsell.text <- function(colour.spec, ...){
  colour.spec <- check.munsell(colour.spec, ...)
  positions <- match(colour.spec, munsell.map$name)
  hex.vals <- munsell.map[positions, "hex"]
  return(hex.vals)
}


# takes hue (character), value(numeric) and chroma(numeric)
# outputs hex
munsell <- function(hue, value, chroma, ...){
  selected <- paste(hue, " ", value, "/", chroma,  sep = "")
  hex.vals <- munsell.text(selected, ...)
  return(hex.vals)
}


rgb2munsell <- function(R, G = NULL, B = NULL){
    LUV.vals <- as(RGB(R, G, B), "LUV")@coords
    ncolors <- nrow(LUV.vals)
    dist.calc <- function(x, y) rowSums((rep(x, each = ncolors) - y) ^ 2)
    dists <- apply(munsell.map[, c("L", "U", "V")], 1, dist.calc, y = LUV.vals)
    if(is.null(dim(dists)))  closest <- which.min(dists)
    else closest <- apply(dists, 1, which.min)
    return(munsell.map[closest, "name"])
}
