# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.0 (2018-04-23) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.2.648)            |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Los_Angeles          |
|date     |2018-06-11                   |

## Packages

|package    |*  |version |date       |source        |
|:----------|:--|:-------|:----------|:-------------|
|colorspace |   |1.3-2   |2016-12-14 |cran (@1.3-2) |
|ggplot2    |   |2.2.1   |2016-12-30 |cran (@2.2.1) |
|munsell    |*  |0.4.3   |2016-02-13 |cran (@0.4.3) |
|testthat   |   |2.0.0   |2017-12-13 |cran (@2.0.0) |

# Check results

6 packages

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|bea.R       |1.0.6   |      0|        0|     0|
|ggraptR     |1.0     |      0|        0|     1|
|mafs        |0.0.3   |      0|        0|     0|
|RGraphics   |2.0-14  |      0|        0|     2|
|scales      |0.5.0   |      1|        0|     0|
|soilprofile |1.0     |      0|        0|     3|

## bea.R (1.0.6)
Maintainer: Andrea Batch <Andrea.Julca@bea.gov>

0 errors | 0 warnings | 0 notes

## ggraptR (1.0)
Maintainer: Eugene Dubossarsky <eugene@presciient.com>  
Bug reports: https://github.com/cargomoose/raptR/issues

0 errors | 0 warnings | 1 note 

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘RSelenium’
```

## mafs (0.0.3)
Maintainer: Sillas Gonzaga <sillas.gonzaga@gmail.com>  
Bug reports: http://github.com/sillasgonzaga/mafs/issues

0 errors | 0 warnings | 0 notes

## RGraphics (2.0-14)
Maintainer: Paul Murrell <paul@stat.auckland.ac.nz>

0 errors | 0 warnings | 2 notes

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘graph’ ‘gWidgetsRGtk2’ ‘hyperdraw’ ‘hypergraph’ ‘playwith’ ‘pmg’
  ‘rggobi’ ‘Rgraphviz’ ‘SVGAnnotation’

checking installed package size ... NOTE
  installed size is  9.9Mb
  sub-directories of 1Mb or more:
    extra   9.4Mb
```

## scales (0.5.0)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/scales/issues

1 error  | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘scales-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: div_gradient_pal
> ### Title: Diverging colour gradient (continous).
> ### Aliases: div_gradient_pal
> 
> ### ** Examples
> 
> x <- seq(-1, 1, length.out = 100)
> r <- sqrt(outer(x^2, x^2, "+"))
> image(r, col = div_gradient_pal()(seq(0, 1, length.out = 12)))
> image(r, col = div_gradient_pal()(seq(0, 1, length.out = 30)))
> image(r, col = div_gradient_pal()(seq(0, 1, length.out = 100)))
> 
> library(munsell)
> image(r, col = div_gradient_pal(low =
+    mnsl(complement("10R 4/6", fix = TRUE)))(seq(0, 1, length = 100)))
Error in complement("10R 4/6", fix = TRUE) : unused argument (fix = TRUE)
Calls: image ... gradient_n_pal -> colour_ramp -> mnsl -> check_mnsl -> na.exclude
Execution halted
```

## soilprofile (1.0)
Maintainer: Gianluca Filippa <gian.filippa@gmail.com>

0 errors | 0 warnings | 3 notes

```
checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘aqp’ ‘lattice’ ‘methods’ ‘munsell’ ‘splancs’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.

checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  head.profile.data.frame plot.profile.data.frame
  print.profile.data.frame summary.profile.data.frame
  tail.profile.data.frame
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.

checking R code for possible problems ... NOTE
eplot: no visible global function definition for ‘trellis.par.get’
eplot: no visible global function definition for ‘trellis.par.set’
eplot: no visible global function definition for ‘xyplot’
head.profile.data.frame: no visible global function definition for
  ‘head’
munsell_to_rgb: no visible global function definition for ‘munsell2rgb’
plot_profile: no visible global function definition for ‘aggregate’
plot_profile: no visible global function definition for ‘layout’
plot_profile: no visible global function definition for ‘lcm’
... 23 lines ...
  ‘tail’
Undefined global functions or variables:
  aggregate areapl axis head layout lcm legend lines mtext munsell2rgb
  na.omit par plot point.in.polygon polygon rnorm runif tail text
  trellis.par.get trellis.par.set xyplot
Consider adding
  importFrom("graphics", "axis", "layout", "lcm", "legend", "lines",
             "mtext", "par", "plot", "polygon", "text")
  importFrom("stats", "aggregate", "na.omit", "rnorm", "runif")
  importFrom("utils", "head", "tail")
to your NAMESPACE file.
```

