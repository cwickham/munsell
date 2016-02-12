## Test environments
* local OS X 10.10.5 install, R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Charlotte Wickham <cwickham@gmail.com>'
  
    License components with restrictions and base license permitting such:
      MIT + file LICENSE
    File 'LICENSE':
      YEAR: 2016
      COPYRIGHT HOLDER: Charlotte Wickham
    
    Possibly mis-spelled words in DESCRIPTION:
      Munsell (3:28, 7:64, 11:35, 12:5)
      Munsell's (8:41)

  Munsell is a proper name and is spelled correctly.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of munsell (soilprofile, scales, Rgraphics)

All packages passed with no ERRORs or WARNINGs.