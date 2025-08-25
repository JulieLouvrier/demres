load("./data/adeliepenguin.rda")
devtools::load_all()

library(popdemo)
library(ggplot2)

set.seed(1234)


## SINGLE POPULATION -----------------------------------------------------------

# not standardized
penguinvec1 <- round(runif(2) * 100)

peng_proj_reg <- popdemo::project(
  adeliepenguin[[1]], standard.A = FALSE, vector = penguinvec1, time = 5
)

# standardized
penguinvec2 <- penguinvec1 / sum(penguinvec1)

peng_proj_std <- popdemo::project(
  adeliepenguin[[1]], standard.A = TRUE, vector = penguinvec2, time = 5
)


# -----------------------------------------------------------------------
# TODO: Julie adds single undisturbed pop projection with explicit names

# Julie:
# extracting the stable stage distributions (aka asymptotic vectors)
# out of the right eigenvector of the matrix
# and multiplying it by the sum of individuals we have in a population
ss_vec1 <- popdemo::eigs(adeliepenguin[[1]])$ss*sum(penguinvec1) # CED: doesnt work for me

#project the undisturbed pop based on the asymptotic initial vectors
projundisturbed1 <-  popdemo::project(
    adeliepenguin,
    vector = ss_vec1,
    standard.A = FALSE,
    time = 5
  )

#Ju: I don't see it here necessary to name it as it is only for one matrix, or?

#(works also for standardized vector)

# -----------------------------------------------------------------------


# absolute .....................................................................

## "original" plot
popdemo::plot(
  peng_proj_reg,
  ylim = c(0, 300)
)

# default for a single (non-standardized) projection:
plot_proj(popvec = peng_proj_reg)

# if you like to adjust limits, use regular ggplot logic:
plot_proj(popvec = peng_proj_reg) +
  coord_cartesian(ylim = c(0, 300))

# all parameters -> those which are not applicable return message
plot_proj(
  popvec = peng_proj_reg,
  standard.A = FALSE, # default
  palette = "red",
  baseline = TRUE,
  sort = TRUE, # will be ignored
  facet = TRUE, # will be ignored
  compare = TRUE # will be ignored
)


# standardized .................................................................

## "original" plot
popdemo::plot(
  peng_proj_std,
  ylim = c(0, 1)
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
  coord_cartesian(ylim = c(0, 1))


# input handling ...............................................................

plot_proj(popvec = penguinvec1)
plot_proj(popvec = peng_proj_std, facet = "ID")


## MULTIPLE POPULATIONS --------------------------------------------------------

pops_proj_reg <- lapply(c(1:length(adeliepenguin)), FUN = function( x ) {(
  popdemo::project(
    adeliepenguin[[x]], standard.A = FALSE, vector = penguinvec1, time = 5
  )
)})

pops_proj_std <- lapply(c(1:length(adeliepenguin)), FUN = function( x ) {(
  popdemo::project(
    adeliepenguin[[x]], standard.A = TRUE, vector = penguinvec2, time = 5
  )
)})

##Julie
names(pops_proj_reg) <- paste("ID", c(1:length(adeliepenguin)))
names(pops_proj_std) <- paste("ID", c(1:length(adeliepenguin)))

#-------------------------------------------------------------------------------
# TODO: Julie adds multiple undisturbed pop projections with explicit names
# Julie:
# extracting the stable stage distributions (aka asymptotic vectors)
# out of the right eigenvector of each matrix
# and multiplying it by the sum of individuals we have in a population
ss_vec <- mapply(function(A, X) {
  (popdemo::eigs(A)$ss)*sum(X)
},
A = adeliepenguin,
X = penguinvec1,
SIMPLIFY = FALSE)

timeproj = 5

#project the undisturbed pop based on the asymptotic initial vectors
projundisturbed <- mapply(function(A, X) {
  popdemo::project(
    A,
    vector = X,
    standard.A = FALSE,
    time = timeproj # CED: timeproj is not defined
  )
},
A = adeliepenguin,
X = ss_vec,
SIMPLIFY = FALSE)

# default naming (in the demres_plot function there is the option to select
# only specific matrices and naming them accordingly)
names(projundisturbed) <- paste("ID", c(1:length(adeliepenguin)))

#(works also for standardized vector)

#-------------------------------------------------------------------------------


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

# we can add a baseline for the initial population size:
plot_proj(
  popvec = pops_proj_reg,
  baseline = TRUE,
  palette = "red"
)

# ... and style it by passing a string:
plot_proj(
  popvec = pops_proj_reg,
  baseline = "red solid"
)

plot_proj(
  popvec = pops_proj_reg,
  baseline = "#ff6633 dotted 1"
)

plot_proj(
  popvec = pops_proj_reg,
  facet = FALSE,
  baseline = "5 dotdash"
)

plot_proj(
  popvec = pops_proj_reg,
  facet = FALSE,
  baseline = "longdot dashed red 2" # ignores words that don't match type or colors
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
