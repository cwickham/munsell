#' Checks for valid Munsell colours
#'
#' Checks user supplied munsell specification for validity.  
#' I.e. colour is of form "h v/c" and h,  v and c take valid values.
#' @param col a character vector representing Munsell colours.
#' @param fix passed on to \code{\link{fix_mnsl}}. Use \code{fix = TRUE} to
#' fix "bad" colours
#' @return  a character vector containing the input colours.  If any colours
#' were outside the gamut they will be represented by NA.
#' @export
#' @examples 
#' check_mnsl(c("5R 5/8","2.5R 9/28"))
#' @keywords internal
#' @importFrom methods as
#' @importFrom stats na.exclude
check_mnsl <- function(col){
  col_na <- na.exclude(col)
  col <- toupper(as.vector(col_na))
  # check format
  right.format <- grep("^[N]|([0-9]?.?[0-9][A-Z]{1,2})[ ][0-9]?.?[0-9]/[0-9]?.?[0-9]{1,2}$",
    col)
  if (length(right.format) != length((col))) {
    if (length(right.format) == 0) {bad.cols <- paste(col,  collapse = ", ")}
    else {bad.cols <- paste(col[-right.format],  collapse = ", ")}
    stop("some of your colours are not correctly formatted:",  bad.cols)
  }
  #check hues
  hues <- gsub("[0-9 /.]", "", col)
  act.hues <- c("N", "R", "YR", "Y", "GY", "G", "BG", "B", "PB", "P", "RP")
  good.hue <-  hues %in% act.hues
  if (!all(good.hue)){
    bad.hue <- paste(hues[!good.hue], "in", col[!good.hue],  
      collapse = "; ")
    act.hue.str <- paste(act.hues, collapse = ", ")
    stop("you have specified invalid hue names: ", bad.hue, 
      "\n hues should be one of ", act.hue.str)
  }
  col.split <- lapply(strsplit(col, "/"), 
     function(x) unlist(strsplit(x, " ")))
  col.split <- lapply(col.split, gsub, pattern = "[A-Z]", replacement = "")
  step <- as.numeric(sapply(col.split, "[", 1))
  values <- as.numeric(sapply(col.split, "[", 2))
  chromas <- as.numeric(sapply(col.split, "[", 3))
  
  act.steps <- c(seq(2.5, 10, by = 2.5),  NA)
  good.step <- step %in% act.steps
  if(!all(good.step)){
    bad.step <- paste(step[!good.step], "in", col[!good.step],  
      collapse = "; ")
    act.step.str <- paste(act.steps,  collapse = ", ")
    stop("you have specified invalid hue steps: ", bad.step, 
       "\n hues steps should be one of ", act.step.str)
  }
  good.value <- values == round(values) & values <= 10 & values >= 0
  if(!all(good.value)) {
    bad.value <- paste(values[!good.value], "in", col[!good.value],  
      collapse = "; ")
    stop("some colours have values that are not integers between 0 and 10: ", bad.value)
  }
  good.chroma <- (chromas %% 2) == 0 
  if(!all(good.chroma)) {
    bad.chroma <- paste(chromas[!good.chroma], "in", col[!good.chroma],  
      collapse = "; ")
    stop("some colours have chromas that are not multiples of two: ",
      bad.chroma)
  }
  na_handle(col_na, col)
}

#' Checks if a Munsell colour is defined in RGB space
#'
#' Not all possible correctly formatted Munsell colours result in a colour
#' representable in RGB space.  This function checks if the colour is
#' representable.  
#' @param col a character vector representing Munsell colours.
#' @param fix passed on to \code{\link{fix_mnsl}}. Use \code{fix = TRUE} to
#' fix "bad" colours
#' @return  a character vector containing the input colours.  If any colours
#' were outside the gamut they will be represented by NA.
#' @export
#' @importFrom stats na.exclude
#' @examples 
#' in_gamut(c("5R 5/8","2.5R 9/28"))
#' @keywords internal
in_gamut <- function(col, fix = FALSE){
  col <- na.exclude(col)
  positions <- match(col, munsell.map$name)
  hex <- munsell.map[positions, "hex"]
  if(any(is.na(hex))){
    bad.colours <- paste(col[is.na(hex)], collapse = ", ")
    if(!fix){
      warning("some specified colours are undefined. You could try fix = TRUE")
      col[is.na(hex)] <- NA
      }
    else{
      col[is.na(hex)] <- fix_mnsl(col[is.na(hex)])
    }
  }
  na_handle(col, as.vector(col))
}
#' Fix an undefined Munsell colour
#'
#' Takes correctly specified but undefined colours and outputs something
#' sensible.  Normally this happens when the chroma is too high.  So,  here
#' sensible means the colour with the same hue and value and maximum defined
#' chroma.
#' @param col a character vector representing Munsell colours.
#' @return  a character vector containing the fixed colours.
#' @export
#' @examples 
#' fix_mnsl(c("5R 5/8","2.5R 9/28"))
#' @keywords internal
fix_mnsl <- function(col){
  col.split <- lapply(strsplit(col, "/"), 
     function(x) unlist(strsplit(x, " ")))
  max.chroma <- function(colour.args){
    hue.value <- munsell.map[munsell.map$hue == colour.args[1] & 
        munsell.map$value == colour.args[2] & !is.na(munsell.map$hex),] 
    hue.value[which.max(hue.value$chroma), "name"]
  }
  unlist(lapply(col.split, max.chroma))
}

#' Munsell hues
#'
#' Returns a character vector of the Munsell hues in hue order starting at 2.5R and excluding grey ("N").
#' @return  a character vector containing the hue values.
#' @export
#' @examples 
#' mnsl_hues()
mnsl_hues <- function(){
 as.character(unique(munsell.map$hue)[-1])
}