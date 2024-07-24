<!-- README.md is generated from README.Rmd. Please edit that file -->

# demres

The goal of **{demres}** is to provide easy functions to calculate
different time-varying and time- constant demographic resilience
metrics. It also allows plotting the resulting metrics and measuring the
discrepancy between the time-varying and the time-constant approach.

The different metrics provided are:  
- Damping ratio  
- Inertia  
- Reactivity  
- Maximum amplification  
- Maximum attenuation

It is build around one dependency:

-   [**{popdemo}**](https://github.com/r-lib/rlang)

**{demres}** aims at being compatible with both *tidyverse* and *base* R
dialects.

## Package installation

You can install this package using **{remotes}** (or **{devtools}**):

``` r
remotes::install_github("JulieLouvrier/demres")
```

## Basic usage of the package

### Load the package {demres}

``` r
library(demres)
```

### Import Matrix Population Models

``` r
# load data
data(bluecrane)
```

### Calculate demographic resilience metrics based on one Matrix

The function `calc_resilience` is made to calculate demographic
resilience metrics based on one Matrix Population Model. It returns a
`dataframe`

``` r
#select the first matrix 
Crane_mat1 <- bluecrane[[1]]

# Create initial vector
set.seed(1234)
Crane_vec1 <- runif(5)
Crane_vec1 <- Crane_vec1 / sum(Crane_vec1) #scales the vec to sum to 1

#let's calculate all metrics of demographic resilience for that one matrix with one initial vector
all_BlueCrane1_demres <- calc_resilience(Crane_mat1, metrics = c("all"),
vector = Crane_vec1, bounds = TRUE, popname = "Blue Crane")
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed

#let's take a look at what we got
all_BlueCrane1_demres
#>      popname       dr  inertia inertia_lwr inertia_upr maxamp maxamp_upr
#> 1 Blue Crane 1.260737 0.881403   0.3380643    2.453536     NA   3.188589
#>      maxatt maxatt_lwr     reac  reac_lwr reac_upr
#> 1 0.5491821  0.1377865 0.729341 0.5324434 2.895077
```

### Calculate demographic resilience metrics based on a list of Matrices

The function `demres` is made to calculate demographic resilience
metrics based on a lost Matrix Population Models. It returns a
`dataframe` with both time-varying and time-constant approaches when
required with the option `time`.

``` r
#let's calculate all metrics of demographic resilience for that one matrix with one initial vector
all_BlueCrane_demres <- demres(bluecrane, metrics = c("all"),
vector = Crane_vec1, bounds = TRUE, popname = "Blue Crane", time = "both")
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed
#> Model does not amplify. Cannot compute maximum amplification, with the stated initial vector, Na is displayed

#let's take a look at what we got
all_BlueCrane_demres
#>    timestep    popname    dr_TV inertia_TV inertia_lwr_TV inertia_upr_TV
#> 1         1 Blue Crane 1.260737  0.8814030      0.3380643       2.453536
#> 2         2 Blue Crane 1.209277  0.8748971      0.3664144       2.764719
#> 3         3 Blue Crane 1.250912  0.8755326      0.3457900       2.481637
#> 4         4 Blue Crane 1.222042  0.8743086      0.3601647       2.662348
#> 5         5 Blue Crane 1.327353  0.8912492      0.3058453       2.216446
#> 6         6 Blue Crane 1.275744  0.8816853      0.3315223       2.380029
#> 7         7 Blue Crane 1.288798  0.8815614      0.3262064       2.321470
#> 8         8 Blue Crane 1.313330  0.8881760      0.3128593       2.252394
#> 9         9 Blue Crane 1.247210  0.8750568      0.3477418       2.500855
#> 10       10 Blue Crane 1.214877  0.8736919      0.3642050       2.713575
#> 11       11 Blue Crane 1.312434  0.8892603      0.3126440       2.259734
#> 12       12 Blue Crane 1.290253  0.8842358      0.3241436       2.326127
#>    maxamp_TV maxamp_upr_TV maxatt_TV maxatt_lwr_TV   reac_TV reac_lwr_TV
#> 1         NA      3.188589 0.5491821     0.1377865 0.7293410   0.5324434
#> 2         NA      3.722920 0.5559752     0.1325322 0.7445792   0.5723503
#> 3         NA      3.238037 0.5497432     0.1393395 0.7318383   0.5436130
#> 4         NA      3.548079 0.5531125     0.1352809 0.7399009   0.5634825
#> 5         NA      2.770840 0.5508220     0.1379891 0.7182240   0.4907642
#> 6         NA      3.061100 0.5489686     0.1392934 0.7264232   0.5239987
#> 7         NA      2.959427 0.5491824     0.1405172 0.7242717   0.5173623
#> 8         NA      2.835762 0.5500417     0.1389008 0.7202318   0.4997366
#> 9         NA      3.271153 0.5499945     0.1390491 0.7327335   0.5462951
#> 10        NA      3.635428 0.5545873     0.1342159 0.7424248   0.5693033
#> 11        NA      2.848023 0.5499636     0.1383543 0.7202490   0.4992444
#> 12        NA      2.966458 0.5491461     0.1393490 0.7238222   0.5142661
#>    reac_upr_TV    dr_TC inertia_TC inertia_lwr_TC inertia_upr_TC maxamp_TC
#> 1     2.895077 1.260737   0.881403      0.3380642       2.453536        NA
#> 2     3.382101 1.260737   0.881403      0.3380642       2.453536        NA
#> 3     2.935359 1.260737   0.881403      0.3380642       2.453536        NA
#> 4     3.219720 1.260737   0.881403      0.3380642       2.453536        NA
#> 5     2.518515 1.260737   0.881403      0.3380642       2.453536        NA
#> 6     2.778008 1.260737   0.881403      0.3380642       2.453536        NA
#> 7     2.684792 1.260737   0.881403      0.3380642       2.453536        NA
#> 8     2.576003 1.260737   0.881403      0.3380642       2.453536        NA
#> 9     2.965338 1.260737   0.881403      0.3380642       2.453536        NA
#> 10    3.299575 1.260737   0.881403      0.3380642       2.453536        NA
#> 11    2.587806 1.260737   0.881403      0.3380642       2.453536        NA
#> 12    2.693034 1.260737   0.881403      0.3380642       2.453536        NA
#>    maxamp_upr_TC maxatt_TC maxatt_lwr_TC  reac_TC reac_lwr_TC reac_upr_TC
#> 1       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 2       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 3       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 4       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 5       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 6       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 7       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 8       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 9       3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 10      3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 11      3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
#> 12      3.188589 0.5491821     0.1377865 0.729341   0.5324433    2.895078
```

### Assessing the distance between the time-varying and the time-constant approaches

The function `demres_dist` calculates the distance between the
time-varying resilience metric and the time-constant one by measuring
the RMSE, rRMSE and the MAPE:

#### RMSE :

(sqrt(mean((TV-TC)^2)) with TV: the time-Varying resilience metric and
TC the time constant one)

#### rRMSE:

(sqrt(mean((TV-TC)^2)) / sd(TV)

#### MAPE”:

(mean(abs(TV - TC))

``` r
#let's calculate all measures of distance for inertia
dist_BC <- demres_dist(table = all_BlueCrane_demres, metric = "inertia", measure = "all")

#let's take a look at what we got
dist_BC
#>                         RMSE     rRMSE        MAPE
#> inertia_lwr      0.020053213 0.9611536 0.017429949
#> inertia_vector 0.006052127 0.9604719 0.005106478
#> inertia_upr      0.178977159 0.9586754 0.151706049
```

### plotting the results

`demres_plot` provides a plot to visually inspect the resilience metric
along a time axis

``` r
#specifying the metric that we want to visually inspect
metric = "inertia"
#plotting
demres_plot(table = all_BlueCrane_demres, metric = metric, plotname = paste0(getwd(), "/plots/plot_demres_", metric, ".pdf"), RMSE = TRUE)
#> quartz_off_screen 
#>                 2
```
