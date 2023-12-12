#' Provides resilience metrics from the package popdemo
#'
#' \code{demres} calculates resilience metrics of a population based
#' on a list of matrix population models
#'
#' This function first checks that the input is a list of matrices,
#' if not, it gives back an error message a
#' if yes it applies the function "wrapper_res_met" to a list of matrices
#'
#'
#' @param listA a square, primitive, irreducible, non-negative numeric matrix of any
#' dimension
#' @param metrics: "reac": Calculates reactivity: first-timestep amplification
#'                 and first-timestep attenuation for a population matrix
#'                 projection model.
#'                 "inertia": Calculates population inertia for a population
#'                 matrix projection model.
#'                 "dr": Calculate the damping ratio of a given population
#'                 matrix projection model.
#'                 "maxamp": Calculate maximal amplification for a population
#'                 matrix projection model.
#'                 "maxatt": Calculate maximal attenuation for a population
#'                 matrix projection model.
#'                 "all": all of the above metrics are provided
#' @param bounds (optional) if TRUE, specifies whether the upper and  lower bound
#' should be calculated
#' if vec is not specified, the function provides metrics in their upper and
#' lower bound, calculated based on the stage-biased vec
#' if vec is specified, the function provides also the metrics calculated based
#' on the inital vec
#' @param initvec a numeric vec or one-column matrix describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific'
#'  maximal amplification
#' @param popname a character string describing the name of the population
#' @param time a character string: "constant" or "varying"
#'            "constant" : if the metrics are to be calculated over the whole study period
#'            "varying": if the metrics are to be calculated for each time step
#' @return A tibble containing all the resilience metrics
#'
#'
#'
#'@name demres


source("R/wrapper_function.r")


demres <- function(listA,
                   metrics,
                   bounds = FALSE,
                   initvec = "n",
                   popname = NULL,
                   time) {

  if(is.list(listA) == T) {
    dem_output <- wrapper_res_met(listA = listA,
                                  metrics = metrics,
                                  bounds = bounds,
                                  initvec = initvec,
                                  popname = popname,
                                  time = time)
  }
  else{
    print("Warning: a list of matrices should be provided")
    listA <- list(listA)
    dem_output <- wrapper_res_met(listA = listA,
                                  metrics = metrics,
                                  bounds = bounds,
                                  initvec = initvec,
                                  popname = popname,
                                  time = time)

  }
  return(dem_output)
}
