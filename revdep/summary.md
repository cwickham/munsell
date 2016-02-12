# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.3 (2015-12-10) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.873)           |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Los_Angeles          |
|date     |2016-02-11                   |

## Packages

|package    |*  |version |date       |source         |
|:----------|:--|:-------|:----------|:--------------|
|colorspace |   |1.2-6   |2015-03-11 |CRAN (R 3.2.0) |
|ggplot2    |   |2.0.0   |2015-12-18 |CRAN (R 3.2.3) |
|testthat   |   |0.11.0  |2015-10-14 |CRAN (R 3.2.0) |

# Check results
3 checked out of 3 dependencies 

## RGraphics (2.0-13)
Maintainer: Paul Murrell <paul@stat.auckland.ac.nz>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘graph’ ‘hyperdraw’ ‘hypergraph’ ‘rggobi’ ‘Rgraphviz’ ‘SVGAnnotation’
```
```
checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    extra   9.4Mb
```
```
checking dependencies in R code ... NOTE
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/private/var/folders/d0/gkc929kn4cn61pz_chjl6hn00000gn/T/Rtmpke7yss/R-lib/RGtk2/libs/RGtk2.so':
  dlopen(/private/var/folders/d0/gkc929kn4cn61pz_chjl6hn00000gn/T/Rtmpke7yss/R-lib/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /private/var/folders/d0/gkc929kn4cn61pz_chjl6hn00000gn/T/Rtmpke7yss/R-lib/RGtk2/libs/RGtk2.so
  Reason: image not found
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/private/var/folders/d0/gkc929kn4cn61pz_chjl6hn00000gn/T/Rtmpke7yss/R-lib/cairoDevice/libs/cairoDevice.so':
  dlopen(/private/var/folders/d0/gkc929kn4cn61pz_chjl6hn00000gn/T/Rtmpke7yss/R-lib/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
  Referenced from: /private/var/folders/d0/gkc929kn4cn61pz_chjl6hn00000gn/T/Rtmpke7yss/R-lib/cairoDevice/libs/cairoDevice.so
  Reason: image not found
JavaVM: requested Java version ((null)) not available. Using Java at "" instead.
JavaVM: Failed to load JVM: /bundle/Libraries/libserver.dylib
JavaVM FATAL: Failed to load the jvm library.
JavaVM: requested Java version ((null)) not available. Using Java at "" instead.
JavaVM: Failed to load JVM: /bundle/Libraries/libserver.dylib
JavaVM FATAL: Failed to load the jvm library.
```
```
DONE
Status: 3 NOTEs
```

## scales (0.3.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/scales/issues

__OK__

## soilprofile (1.0)
Maintainer: Gianluca Filippa <gian.filippa@gmail.com>

```
checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘aqp’ ‘lattice’ ‘methods’ ‘munsell’ ‘splancs’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  head.profile.data.frame plot.profile.data.frame
  print.profile.data.frame summary.profile.data.frame
  tail.profile.data.frame
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
eplot: no visible global function definition for ‘trellis.par.get’
eplot: no visible global function definition for ‘trellis.par.set’
eplot: no visible global function definition for ‘xyplot’
munsell_to_rgb: no visible global function definition for ‘munsell2rgb’
roots: no visible global function definition for ‘areapl’
roots: no visible global function definition for ‘point.in.polygon’
skeletal: no visible global function definition for ‘areapl’
skeletal: no visible global function definition for ‘point.in.polygon’
```
```
DONE
Status: 3 NOTEs
```

