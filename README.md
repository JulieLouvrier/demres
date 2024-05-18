<!-- README.md is generated from README.Rmd. Please edit that file -->

# demres

The goal of **{demres}** is to provide easy functions to calculate
different time-varying and time- constant demographic resilience
metrics. It also allows plotting the resulting metrics and measuring the
discrepancy between the time-varying and the time-constant approach.

The different metrics provided are: - Damping ratio - Inertia \_
Reactivity - Maximum amplification - Maximum attenuation

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
comadre <- Rcompadre::cdb_fetch("comadre")
```

    ## This is COMADRE version 4.23.3.1 (release date Mar_24_2023)
    ## See user agreement at https://compadre-db.org/Help/UserAgreement
    ## See how to cite with `citation(Rcompadre)`

``` r
#selecting the blue crane
blue_crane <- comadre[comadre@data$SpeciesAccepted  == "Anthropoides paradiseus", ]

#extracting matrices
blue_crane_matA <- Rcompadre::matA(blue_crane)
```

### Calculate demographic resilience metrics based on one Matrix

The function `calc_resilience` is made to calculate demographic
resilience metrics based on one Matrix Population Model. It returns a
`dataframe`

``` r
#select the first matrix 
Crane_mat1 <- blue_crane_matA[[1]]

# Create initial vector
Crane_vec1 <- runif(5)
Crane_vec1 <- Crane_vec1 / sum(Crane_vec1) #scales the vec to sum to 1

#let's calculate all metrics of demographic resilience for that one matrix with one initial vector
all_BlueCrane1_demres <- calc_resilience(Crane_mat1, metrics = c("all"),
initvec = Crane_vec1, bounds = TRUE, popname = "Blue Crane")

#let's take a look at what we got
all_BlueCrane1_demres
```

    ##      popname       dr  inertia inertia_lwr inertia_upr   maxamp maxamp_upr
    ## 1 Blue Crane 1.260737 1.503012   0.3380643    2.453536 1.747398   3.188589
    ##   maxatt maxatt_lwr     reac  reac_lwr reac_upr
    ## 1     NA  0.1377865 1.124699 0.5324434 2.895077

### Calculate demographic resilience metrics based on a list of Matrices

The function `demres` is made to calculate demographic resilience
metrics based on a lost Matrix Population Models. It returns a
`dataframe` with both time-varying and time-constant approaches when
required with the option `time`.

``` r
#let's calculate all metrics of demographic resilience for that one matrix with one initial vector
all_BlueCrane_demres <- demres(blue_crane_matA, metrics = c("all"),
initvec = Crane_vec1, bounds = TRUE, popname = "Blue Crane", time = "both")
```

    ## Model does not attenuate.  Cannot compute maximum attenuation, with the stated initial vector, Na is displayed

``` r
#let's take a look at what we got
all_BlueCrane_demres
```

    ##    timestep    popname    dr_TV inertia_TV inertia_lwr_TV inertia_upr_TV
    ## 1         1 Blue Crane 1.260737   1.503012      0.3380643       2.453536
    ## 2         2 Blue Crane 1.209277   1.565150      0.3664144       2.764719
    ## 3         3 Blue Crane 1.250912   1.501148      0.3457900       2.481637
    ## 4         4 Blue Crane 1.222042   1.541009      0.3601647       2.662348
    ## 5         5 Blue Crane 1.327353   1.461074      0.3058453       2.216446
    ## 6         6 Blue Crane 1.275744   1.486206      0.3315223       2.380029
    ## 7         7 Blue Crane 1.288798   1.472292      0.3262064       2.321470
    ## 8         8 Blue Crane 1.313330   1.465281      0.3128593       2.252394
    ## 9         9 Blue Crane 1.247210   1.504921      0.3477418       2.500855
    ## 10       10 Blue Crane 1.214877   1.551760      0.3642050       2.713575
    ## 11       11 Blue Crane 1.312434   1.468508      0.3126440       2.259734
    ## 12       12 Blue Crane 1.290253   1.477137      0.3241436       2.326127
    ##    maxamp_TV maxamp_upr_TV maxatt_TV maxatt_lwr_TV  reac_TV reac_lwr_TV
    ## 1   1.747398      3.188589        NA     0.1377865 1.124699   0.5324434
    ## 2   1.867999      3.722920        NA     0.1325322 1.197573   0.5723503
    ## 3   1.749837      3.238037        NA     0.1393395 1.128988   0.5436130
    ## 4   1.824857      3.548079        NA     0.1352809 1.171918   0.5634825
    ## 5   1.651613      2.770840        NA     0.1379891 1.073767   0.4907642
    ## 6   1.714607      3.061100        NA     0.1392934 1.107649   0.5239987
    ## 7   1.687405      2.959427        NA     0.1405172 1.094256   0.5173623
    ## 8   1.664682      2.835762        NA     0.1389008 1.080839   0.4997366
    ## 9   1.757416      3.271153        NA     0.1390491 1.133324   0.5462951
    ## 10  1.844832      3.635428        NA     0.1342159 1.184204   0.5693033
    ## 11  1.669779      2.848023        NA     0.1383543 1.082771   0.4992444
    ## 12  1.693721      2.966458        NA     0.1393490 1.096176   0.5142661
    ##    reac_upr_TV    dr_TC inertia_TC inertia_lwr_TC inertia_upr_TC maxamp_TC
    ## 1     2.895077 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 2     3.382101 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 3     2.935359 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 4     3.219720 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 5     2.518515 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 6     2.778008 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 7     2.684792 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 8     2.576003 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 9     2.965338 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 10    3.299575 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 11    2.587806 1.260737   1.503012      0.3380642       2.453536  1.747398
    ## 12    2.693034 1.260737   1.503012      0.3380642       2.453536  1.747398
    ##    maxamp_upr_TC maxatt_TC maxatt_lwr_TC  reac_TC reac_lwr_TC reac_upr_TC
    ## 1       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 2       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 3       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 4       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 5       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 6       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 7       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 8       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 9       3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 10      3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 11      3.188589        NA     0.1377865 1.124699   0.5324433    2.895078
    ## 12      3.188589        NA     0.1377865 1.124699   0.5324433    2.895078

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
```

    ##                        RMSE     rRMSE       MAPE
    ## inertia_lwr      0.02005321 0.9611536 0.01742995
    ## inertia_initvect 0.03410388 0.9617239 0.02835230
    ## inertia_upr      0.17897716 0.9586754 0.15170605

### plotting the results

`demres_plot` provides a plot to visually inspect the resilience metric
along a time axis

``` r
#specifying the metric that we want to visually inspect
metric = "inertia"
#plotting
demres_plot(table = all_BlueCrane_demres, metric = metric, plotname = paste0(getwd(), "/plots/plot_demres_", metric, ".pdf"), RMSE = TRUE)
```

    ## png 
    ##   2
