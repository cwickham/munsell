# function to check user supplied munsell specification
# i.e. colour is of form h v/c 
check.munsell <- function(colour.spec,  fix = FALSE){
  missing <- is.na(colour.spec)
  colour.spec <- toupper(colour.spec[!missing])
  # check format
  right.format <- grep("^[0-9]?.?[0-9][A-Z]{1,2}[ ][0-9]?.?[0-9]/[0-9]?.?[0-9]{1,2}$",
    colour.spec)
  if (length(right.format) != length((colour.spec))) {
   bad.cols <- paste(colour.spec[-right.format],  sep = ", ")
    stop(paste("some of your colours are not correctly formatted:",  bad.cols))  
  }
  #check hues
  hues <- gsub("[0-9 /.]", "", colour.spec)
  act.hues <- c("R", "YR", "Y", "GY", "G", "BG", "B", "PB", "P", "RP")
  good.hue <-  hues %in% act.hues
  if (!all(good.hue)){
    bad.hue <- paste(hues[!good.hue], "in", colour.spec[!good.hue],  
      collapse = "; ")
    stop(paste("you have specified invalid hue names: ", bad.hue, 
      "\n hues should be one of", paste(act.hues,  collapse = ", ")))
  }
  col.split <- lapply(strsplit(colour.spec, "/"), 
     function(x) unlist(strsplit(x, " ")))
  col.split <- lapply(col.split, gsub, pattern = "[A-Z]", replacement = "")
  step <- as.numeric(sapply(col.split, "[", 1))
  values <- as.numeric(sapply(col.split, "[", 2))
  chromas <- as.numeric(sapply(col.split, "[", 3))
  
  act.steps <- seq(2.5, 10, by = 2.5)
  good.step <- step %in% act.steps
  if(!all(good.step)){
    bad.step <- paste(step[!good.step], "in", colour.spec[!good.step],  
      collapse = "; ")
    stop(paste("you have specified invalid hue steps: ", bad.step, 
       "\n hues steps should be one of", paste(act.steps,  collapse = ", ")))
  }
  good.value <- values == round(values) & values <= 10 & values >= 0
  if(!all(good.value)) {
    bad.value <- paste(values[!good.value], "in", colour.spec[!good.value],  
      collapse = "; ")
    stop(paste("some colours have do not have integer values between 0 and 10: ", bad.value))
  }
  good.chroma <- (chromas %% 2) == 0 
  if(!all(good.chroma)) {
    bad.chroma <- paste(chromas[!good.chroma], "in", colour.spec[!good.chroma],  
      collapse = "; ")
    stop(paste("some colours have chromas that aren't multiples of two: ",
      bad.chroma))
  }
  colour.spec <- in.gamut(colour.spec,  fix = fix)
  result <- rep(NA,  length(missing))
  result[!missing] <- colour.spec
  result
}

# function to check colour is defined
in.gamut <- function(colour.spec, fix = FALSE){
  positions <- match(colour.spec, munsell.map$name)
  hex <- munsell.map[positions, "hex"]
  if(any(is.na(hex))){
    bad.colours <- paste(colour.spec[is.na(hex)], collapse = ", ")
    if(!fix){
      warning("some specified colours are undefined. You could try fix = TRUE")
      colour.spec[is.na(hex)] <- NA
      }
    else{
      colour.spec[is.na(hex)] <- fix.munsell(colour.spec[is.na(hex)])
    }
  }
  colour.spec
}

# function that take correctly specified but undefined colours and outputs 
# something sensible - value should be ok - take highest chroma defined
fix.munsell <- function(colour.spec){
  col.split <- lapply(strsplit(colour.spec, "/"), 
     function(x) unlist(strsplit(x, " ")))
  max.chroma <- function(colour.args){
    hue.value <- subset(munsell.map, hue == colour.args[1] & 
        value == colour.args[2] & !is.na(hex)) 
    hue.value[which.max(hue.value$chroma), "name"]
  }
  unlist(lapply(col.split, max.chroma))
}
