<!-- README.md is generated from README.Rmd. Please edit that file -->

# demres

The goal of **{demres}** is to provide easy functions to calculate
different time-varying and time-constant demographic resilience metrics.
It also allows plotting the resulting metrics and measuring the
discrepancy between the time-varying and the time-constant approach.

The different metrics provided are:  
- Convergence time  
- Damping ratio  
- Reactivity  
- Maximum amplification  
- Maximum attenuation

It is build around one (direct) dependency:

- [**{popdemo}**](https://github.com/iainmstott/popdemo)

**{demres}** aims at being compatible with both *tidyverse* and *base* R
dialects.

## Package installation

You can install this package using **{remotes}** (or **{devtools}**):

``` r
remotes::install_github("JulieLouvrier/demres@V1.1")
```

## Basic usage of the package

### Load the package {demres}

``` r
library(demres)
```

### Import Matrix Population Models

``` r
# load data
data(adeliepenguin)
```

### Calculate demographic resilience metrics based on a list of population matrices

The function `resilience` is made to calculate demographic resilience
metrics based on a list of matrix projection models. It returns a
`dataframe` with both time-varying and time-constant approaches when
requested using the argument `time`.

``` r
# Create initial vector
set.seed(125435)
vec <- round(runif(2) * 100 )
vector_list <- lapply(1:28, function(i) vec)

#let's calculate all metrics of demographic resilience  with one initial vector
AP_demres <-
   resilience(
     listA = adeliepenguin,
     metrics = "all",
     vector = vector_list,
     TDvector = FALSE,
     popname = "adelie penguin",
     verbose = FALSE,
     return.N = TRUE,
     return.t = TRUE
   )

#let's take a look at what we got
AP_demres
#>    timestep        popname convt   convt.N       dr maxamp maxamp.t maxatt
#> 1         1 adelie penguin     2  76.18020 4.844809     NA       NA  65.82
#> 2         2 adelie penguin     2  82.60800 3.934655     NA       NA  69.75
#> 3         3 adelie penguin     2  89.71350 4.363470     NA       NA  72.45
#> 4         4 adelie penguin     3  81.27899 7.288792     NA       NA  62.31
#> 5         5 adelie penguin     3  98.97264 5.032717     NA       NA  68.40
#> 6         6 adelie penguin     3  61.94628 6.026819     NA       NA  57.69
#> 7         7 adelie penguin     2  46.87440 8.606140     NA       NA  50.64
#> 8         8 adelie penguin     3  86.25449 6.302694     NA       NA  64.23
#> 9         9 adelie penguin     3 112.26682 4.657293     NA       NA  71.52
#> 10       10 adelie penguin     2  85.31400 4.608732     NA       NA  70.80
#> 11       11 adelie penguin     2  78.62280 4.779472     NA       NA  66.99
#> 12       12 adelie penguin     2  94.22700 4.500000     NA       NA  74.55
#> 13       13 adelie penguin     3  55.74381 4.943374     NA       NA  56.34
#> 14       14 adelie penguin     2 102.98220 3.907040  79.26        1     NA
#> 15       15 adelie penguin     1  78.75000 3.922862     NA       NA  78.75
#> 16       16 adelie penguin     2  62.72070 3.952740     NA       NA  60.51
#> 17       17 adelie penguin     1  71.91000 3.632194     NA       NA  71.91
#> 18       18 adelie penguin     3  56.94250 6.180351     NA       NA  56.16
#> 19       19 adelie penguin     2  75.71310 4.899730     NA       NA  65.61
#> 20       20 adelie penguin     2  57.48480 3.692842     NA       NA  57.84
#> 21       21 adelie penguin     2  86.12250 3.205801     NA       NA  70.95
#> 22       22 adelie penguin     2  65.73960 3.732414     NA       NA  62.28
#> 23       23 adelie penguin     2  79.82100 4.320532     NA       NA  67.53
#> 24       24 adelie penguin     3  58.36334 7.540645     NA       NA  56.16
#> 25       25 adelie penguin     2  79.52820 3.854944     NA       NA  68.07
#> 26       26 adelie penguin     3  47.89507 8.396133     NA       NA  52.14
#> 27       27 adelie penguin     1  67.68000 3.226423     NA       NA  67.68
#> 28       28 adelie penguin     1  67.29000 3.966774     NA       NA  67.29
#>    maxatt.t  reac reac.t
#> 1         1 65.82      1
#> 2         1 69.75      1
#> 3         1 72.45      1
#> 4         1 62.31      1
#> 5         1 68.40      1
#> 6         1 57.69      1
#> 7         1 50.64      1
#> 8         1 64.23      1
#> 9         1 71.52      1
#> 10        1 70.80      1
#> 11        1 66.99      1
#> 12        1 74.55      1
#> 13        1 56.34      1
#> 14       NA 79.26      1
#> 15        1 78.75      1
#> 16        1 60.51      1
#> 17        1 71.91      1
#> 18        1 56.16      1
#> 19        1 65.61      1
#> 20        1 57.84      1
#> 21        1 70.95      1
#> 22        1 62.28      1
#> 23        1 67.53      1
#> 24        1 56.16      1
#> 25        1 68.07      1
#> 26        1 52.14      1
#> 27        1 67.68      1
#> 28        1 67.29      1
```

### Assess the distance between the time-varying and the time-constant approaches

The function `summary` calculates the variation in the time-varying
metrics:

``` r
#let's calculate the default variation measures, namely mean and sd 
summary_AP <- summary(AP_demres)

#let's take a look at what we got
summary_AP
#>               mean         sd
#> convt     2.178571  0.6696362
#> convt.N  75.319498 16.3212880
#> dr        4.940014  1.4940121
#> maxamp   79.260000         NA
#> maxamp.t  1.000000         NA
#> maxatt   64.976667  7.0857343
#> maxatt.t  1.000000  0.0000000
#> reac     65.486786  7.4588397
```

### Plot the results

Function `plot` provides a plot to visually inspect the resilience
metric along a time axis

``` r
plot(AP_demres,
     listA = adeliepenguin,
     vector = vector_list,
     timeproj = 5,
     sort = FALSE,
     compare = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r

 # One plot to compare them all together
 plot(AP_demres,
     listA = adeliepenguin,
     vector = vector_list,
     timeproj = 5,
     sort = TRUE,
     compare = TRUE,
     facet = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
     
 # Compare with the standardized value
plot(AP_demres,
     listA = adeliepenguin,
     vector = vector_list,
     timeproj = 5,
     standard.A = TRUE,
     standard.vec = TRUE,
     sort = TRUE,
     compare = TRUE,
     facet = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->
