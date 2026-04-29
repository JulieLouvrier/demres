<!-- README.md is generated from README.Rmd. Please edit that file -->

# demres

The goal of **{demres}** is to provide functions to calculate a set of
time-varying and time-constant demographic resilience metrics. It also
allows plotting the resulting metrics and measuring the discrepancy
between the time-varying and the time-constant approach.

The different metrics provided are:  
- Convergence time  
- Time to abundance target - Damping ratio  
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
vector_list <- lapply(1:28, function(i) vec) # repeating the initial vector 28 times, for each matrix

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
     return.t = TRUE,
     target.N = 500
   )

#let's take a look at what we got
AP_demres
#>    timestep        popname convt   convt.N   tt target.N       dr maxamp
#> 1         1 adelie penguin     2  76.18020   17      500 4.844809     NA
#> 2         2 adelie penguin     2  82.60800   14      500 3.934655     NA
#> 3         3 adelie penguin     2  89.71350   12      500 4.363470     NA
#> 4         4 adelie penguin     3  81.27899   19      500 7.288792     NA
#> 5         5 adelie penguin     3  98.97264   13      500 5.032717     NA
#> 6         6 adelie penguin     3  61.94628   73      500 6.026819     NA
#> 7         7 adelie penguin     2  46.87440   NA      500 8.606140     NA
#> 8         8 adelie penguin     3  86.25449   17      500 6.302694     NA
#> 9         9 adelie penguin     3 112.26682   11      500 4.657293     NA
#> 10       10 adelie penguin     2  85.31400   13      500 4.608732     NA
#> 11       11 adelie penguin     2  78.62280   16      500 4.779472     NA
#> 12       12 adelie penguin     2  94.22700   11      500 4.500000     NA
#> 13       13 adelie penguin     3  55.74381   NA      500 4.943374     NA
#> 14       14 adelie penguin     2 102.98220    9      500 3.907040  79.26
#> 15       15 adelie penguin     1  78.75000    9      500 3.922862     NA
#> 16       16 adelie penguin     2  62.72070   72      500 3.952740     NA
#> 17       17 adelie penguin     1  71.91000   13      500 3.632194     NA
#> 18       18 adelie penguin     3  56.94250 1015      500 6.180351     NA
#> 19       19 adelie penguin     2  75.71310   18      500 4.899730     NA
#> 20       20 adelie penguin     2  57.48480   NA      500 3.692842     NA
#> 21       21 adelie penguin     2  86.12250   13      500 3.205801     NA
#> 22       22 adelie penguin     2  65.73960   44      500 3.732414     NA
#> 23       23 adelie penguin     2  79.82100   15      500 4.320532     NA
#> 24       24 adelie penguin     3  58.36334  153      500 7.540645     NA
#> 25       25 adelie penguin     2  79.52820   16      500 3.854944     NA
#> 26       26 adelie penguin     3  47.89507   NA      500 8.396133     NA
#> 27       27 adelie penguin     1  67.68000   19      500 3.226423     NA
#> 28       28 adelie penguin     1  67.29000   20      500 3.966774     NA
#>    maxamp.t maxatt maxatt.t  reac reac.t
#> 1        NA  65.82        1 65.82      1
#> 2        NA  69.75        1 69.75      1
#> 3        NA  72.45        1 72.45      1
#> 4        NA  62.31        1 62.31      1
#> 5        NA  68.40        1 68.40      1
#> 6        NA  57.69        1 57.69      1
#> 7        NA  50.64        1 50.64      1
#> 8        NA  64.23        1 64.23      1
#> 9        NA  71.52        1 71.52      1
#> 10       NA  70.80        1 70.80      1
#> 11       NA  66.99        1 66.99      1
#> 12       NA  74.55        1 74.55      1
#> 13       NA  56.34        1 56.34      1
#> 14        1     NA       NA 79.26      1
#> 15       NA  78.75        1 78.75      1
#> 16       NA  60.51        1 60.51      1
#> 17       NA  71.91        1 71.91      1
#> 18       NA  56.16        1 56.16      1
#> 19       NA  65.61        1 65.61      1
#> 20       NA  57.84        1 57.84      1
#> 21       NA  70.95        1 70.95      1
#> 22       NA  62.28        1 62.28      1
#> 23       NA  67.53        1 67.53      1
#> 24       NA  56.16        1 56.16      1
#> 25       NA  68.07        1 68.07      1
#> 26       NA  52.14        1 52.14      1
#> 27       NA  67.68        1 67.68      1
#> 28       NA  67.29        1 67.29      1
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
