load("./data/adeliepenguin.rda")
#devtools::load_all()

library(popdemo)
library(ggplot2)

set.seed(123456)

# pop vect generated an not standardized
penguinvec1 <- round(runif(2) * 100)


## SINGLE POPULATION -----------------------------------------------------------

## projected population
peng_proj_reg <- popdemo::project(
  adeliepenguin[[1]], standard.A = FALSE, vector = penguinvec1, time = 5
)
peng_proj_std <- popdemo::project(
  adeliepenguin[[1]], standard.A = TRUE, vector = penguinvec1, time = 5
)


# absolute
popdemo::plot(
  peng_proj_reg,
  ylim = c(0, 300)
)

plot_results(
    popvec = peng_proj_reg,
    standard.A = FALSE # default
  )

plot_results(
    popvec = peng_proj_reg,
    standard.A = FALSE # default
  ) +
  coord_cartesian(ylim = c(0, 300))

plot_results(
    popvec = peng_proj_reg,
    palette = "red",
    sort = TRUE, # will be ignored
    facet = TRUE, # will be ignored
    compare = TRUE  # will be ignored
  )


# standardized
popdemo::plot(
  peng_proj_std,
  ylim = c(0, 100)
)

plot_results(
    popvec = peng_proj_std,
    standard.A = TRUE
  )


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

my_colors <- viridis::mako(n = length(pops_proj_std), begin = .1, end = .8)

# absolute
# popdemo::plot(
#   pops_proj_reg,
#   ylim = c(0, 300)
# )

plot_results(
  popvec = pops_proj_reg
)

plot_results(
  popvec = pops_proj_reg,
  palette = my_colors
)

plot_results(
  popvec = pops_proj_reg,
  facet = TRUE
)

plot_results(
  popvec = pops_proj_reg,
  facet = TRUE,
  compare = TRUE
)

plot_results(
  popvec = pops_proj_reg,
  facet = TRUE,
  compare = TRUE,
  palette = my_colors
)

plot_results(
  popvec = pops_proj_reg,
  facet = TRUE,
  sort = TRUE,
  compare = TRUE,
  palette = my_colors
)

# standardized
# popdemo::plot(
#   pops_proj_std,
#   ylim = c(0, 100)
# )

plot_results(
    popvec = pops_proj_std,
    standard.A = TRUE
  )

plot_results(
    popvec = pops_proj_std,
    standard.A = TRUE,
    palette = my_colors
  )

plot_results(
    popvec = pops_proj_std,
    standard.A = TRUE,
    facet = TRUE
  )

plot_results(
    popvec = pops_proj_std,
    standard.A = TRUE,
    facet = TRUE,
    compare = TRUE
  )

plot_results(
    popvec = pops_proj_std,
    standard.A = TRUE,
    facet = TRUE,
    compare = TRUE,
    palette = my_colors
  )


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
