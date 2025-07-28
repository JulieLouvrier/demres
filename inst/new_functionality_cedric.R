load("./data/adeliepenguin.rda")
#devtools::load_all()

library(popdemo)
library(ggplot2)

set.seed(1234)

# pop vect generated (not standardized)
penguinvec1 <- round(runif(2) * 100)


## SINGLE POPULATION -----------------------------------------------------------

## projected population
peng_proj_reg <- popdemo::project(
  adeliepenguin[[1]], standard.A = FALSE, vector = penguinvec1, time = 5
)
peng_proj_std <- popdemo::project(
  adeliepenguin[[1]], standard.A = TRUE, vector = penguinvec1, time = 5
)


# absolute .....................................................................

## "original" plot
popdemo::plot(
  peng_proj_reg,
  ylim = c(0, 300)
)

# default for a single (non-standardized) projection:
plot_proj(popvec = peng_proj_reg)

# if you liek to adjust limits, use regular ggplot logic:
plot_proj(popvec = peng_proj_reg) +
  coord_cartesian(ylim = c(0, 300))

# all parameters -> those which are not applicable return message
plot_proj(
  popvec = peng_proj_reg,
  standard.A = FALSE, # default
  palette = "red",
  sort = TRUE, # will be ignored
  facet = TRUE, # will be ignored
  compare = TRUE  # will be ignored
)


# standardized .................................................................

## "original" plot
popdemo::plot(
  peng_proj_std,
  ylim = c(0, 100)
)

plot_proj(
  popvec = peng_proj_std,
  standard.A = TRUE # overwrite default
)

plot_proj(
    popvec = peng_proj_std,
    standard.A = TRUE, # overwrite default
    palette = "royalblue"
  ) +
  coord_cartesian(ylim = c(0, 100))


## MULTIPLE POPULATIONS --------------------------------------------------------

pops_proj_reg <- lapply(c(1:length(adeliepenguin)), FUN = function( x ) {(
  popdemo::project(
    adeliepenguin[[x]], standard.A = FALSE, vector = penguinvec1, time = 5
  )
)})

pops_proj_std <- lapply(c(1:length(adeliepenguin)), FUN = function( x ) {(
  popdemo::project(
    adeliepenguin[[x]], standard.A = TRUE, vector = penguinvec1, time = 5
  )
)})

my_colors <- viridis::rocket(n = length(pops_proj_std), begin = .1, end = .8)

my_colors_named <- my_colors
names(my_colors_named) <- 1:28

# absolute .....................................................................

## function detects if multiple years are passed
## -> now uses facet and compare as defaults
plot_proj(popvec = pops_proj_reg)

## if you like to add color nevertheless:
plot_proj(
  popvec = pops_proj_reg,
  palette = my_colors
)

## if you don't like the comparison lines:
plot_proj(
  popvec = pops_proj_reg,
  compare = FALSE
)

## if you'd like to get all in a single plot:
## -> now with the blue shades as default
plot_proj(
  popvec = pops_proj_reg,
  facet = FALSE
)

## we can rank the lines (note that colors now encode the ranks now, not the
## time steps in increasing order anymore - use custom named vector if you
## wish to do so, example below)
plot_proj(
  popvec = pops_proj_reg,
  facet = FALSE,
  sort = TRUE
)

## use custom color palette (unnamed vector):
plot_proj(
  popvec = pops_proj_reg,
  facet = FALSE,
  sort = TRUE,
  palette = my_colors
)

## named vector instead to keep original sorting by time step:
plot_proj(
  popvec = pops_proj_reg,
  facet = FALSE,
  sort = TRUE,
  palette = my_colors_named
)


## we can sort them in the facet view as well:
plot_proj(
  popvec = pops_proj_reg,
  sort = TRUE,
  palette = my_colors_named
)

plot_proj(
  popvec = pops_proj_reg,
  sort = TRUE,
  palette = "red"
)


# standardized .................................................................

plot_proj(
  popvec = pops_proj_std,
  sort = TRUE
)

plot_proj(
  popvec = pops_proj_std,
  facet = FALSE,
  sort = TRUE
)



## for labeling (ignore for now)

vector_list <- lapply(1:28, function(i) penguinvec1)


AP_demres <- resilience(
  listA = adeliepenguin,
  metrics = "all",
  vector = vector_list,
  TDvector = TRUE,
  popname = "adelie penguin",
  verbose = FALSE,
  return.N = TRUE,
  return.t = TRUE
)
