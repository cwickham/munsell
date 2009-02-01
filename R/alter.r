#TO DO: fix lighter/darker etc. to warn if already lightest etc.

#lighter and darker functions
lighter <- function(col.name){
  col.split <- lapply(strsplit(col.name, "/"), 
    function(x) unlist(strsplit(x, " ")))
  col.spec <- unlist(lapply(col.split, function(x) 
    paste(x[1], " ", as.numeric(x[2]) + 1,"/", x[3] , sep = "")))  
}

darker <- function(col.name){
  col.split <- lapply(strsplit(col.name, "/"), 
    function(x) unlist(strsplit(x, " ")))
  unlist(lapply(col.split, function(x) 
    paste(x[1], " ", as.numeric(x[2]) - 1,"/", x[3] , sep = "")))  
}

# saturate and desaturate
saturate <- function(col.name){
  col.split <- lapply(strsplit(col.name, "/"), 
    function(x) unlist(strsplit(x, " ")))
  unlist(lapply(col.split, function(x) 
    paste(x[1], " ", x[2], "/", as.numeric(x[3]) + 2, sep = "")))  
}

desaturate <- function(col.name){
  col.split <- lapply(strsplit(col.name, "/"), 
    function(x) unlist(strsplit(x, " ")))
  unlist(lapply(col.split, function(x) 
    paste(x[1], " ", x[2], "/", as.numeric(x[3]) - 2, sep = "")))  
}


complement <- function(col.name, ...){
  col.name <- check.munsell(col.name, ...)
  col.split <- lapply(strsplit(col.name, "/"), 
    function(x) unlist(strsplit(x, " ")))
  hues <- levels(munsell.map$hue)[-1]

  comps <- unlist(lapply(col.split, function(x) {
      hue.index <- match(x[1],  hues)
      paste(hues[(hue.index + 20) %% 40], " ", x[2], "/", x[3], sep = "")
    }))
  in.gamut(comps, ...)
}

seq.munsell <- function(from, to, n){
  in.LUV <- munsell.map[match(c(from, to), munsell.map$name), c("L", "U", "V")]
  LUV.seq <- matrix(c(seq(in.LUV$L[1], in.LUV$L[2],  length = n), 
    seq(in.LUV$U[1], in.LUV$U[2],  length = n), 
    seq(in.LUV$V[1], in.LUV$V[2],  length = n)),  ncol = 3)
  rgb2munsell(as(LUV(LUV.seq), "RGB")@coords)
}