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

1 packages with problems

|package |version | errors| warnings| notes|
|:-------|:-------|------:|--------:|-----:|
|scales  |0.5.0   |      1|        0|     0|

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

