#' Munsell colour system.
#' 
#' @description 
#' This package makes it easy to access and manipulate the colours in the 
#' munsell colour system. The conversion from munsell specifications to sRGB based on the renotation data from  \url{http://www.cis.rit.edu/mcsl/online/munsell.php} which is a digitization of Table 1 in Newhall, Nickerson & Judd (1943).  The code for conversion can be found in the package directory in inst/raw/getmunsellmap.r
#' @references S. M. Newhall, D. Nickerson, and D. B. Judd. Final report of the O.S.A. subcommittee on the spacing of the munsell colors. J. Opt. Soc. Am., 33(7):385-411, 07 1943.
#' @references Munsell Renotation Data, RIT Munsell Color Science Laboratory.  \url{http://www.cis.rit.edu/mcsl/online/munsell.php}
#' @docType package
#' @name munsell
#' @aliases munsell package-munsell
#' @import colorspace
NULL


globalVariables(c("hue", "value", "chroma", "name",
  "x", "y", "text.colour", "colour"))