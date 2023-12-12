#' Provides resilience metrics from the package popdemo
#'
#' \code{wrapper_res_met} calculates resilience metrics of a population based
#' on a list of matrix population models
#'
#' This function applies the function "calc_resilience_function" to a list of matrices
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
#'@name wrapper_res_met
#'
#'
source("R/calc_resilience_function.r")

wrapper_res_met <-
  function(listA,
           metrics,
           bounds = FALSE,
           initvec = "n",
           popname = NULL,
           time) {

        if(length(time) == 2) {
        temp_list <-
          lapply(
            listA,
            calc_resilience,
            metrics = metrics,
            bounds = bounds,
            initvec = initvec,
            popname = popname
          )
        met <- do.call(rbind.data.frame, temp_list)
        names(met)[-1] <- paste0(names(met[-1]), "_TV")
        met <- cbind(timestep = c(1:nrow(met)), met)

      meanA <- apply(simplify2array(listA), 1:2, mean)
      res <- calc_resilience(meanA, metrics, bounds, initvec, popname)
      res <- res[,-1]
      names(res) <- paste0(names(res), "_TC")
      met <- cbind(met, res)
    }
    else {
      if (time == "varying"){
        temp_list <-
          lapply(
            listA,
            calc_resilience,
            metrics = metrics,
            bounds = bounds,
            initvec = initvec,
            popname = popname
          )
        met <- do.call(rbind.data.frame, temp_list)
        names(met)[-1] <- paste0(names(met[-1]), "_TV")
        met <- cbind(timestep = c(1:nrow(met)), met)


      }
      if(time == "constant") {
        meanA <- apply(simplify2array(listA), 1:2, mean)
        res <- calc_resilience(meanA, metrics, bounds, initvec, popname)
        names(res)[-1] <- paste0(names(res)[-1], "_TC")
        met <- res
      }
    }

    return(met)
  }


