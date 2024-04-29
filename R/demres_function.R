#' Provides time-varying and time-constant resilience metrics for animal
#' populations based on a list of matrix population models
#'
#' \code{demres} calculates resilience metrics of a population based
#' on a list of matrix population models
#'
#' This function applies the function "calc_resilience" to a list of matrices
#' and returns either time-varying metrics or time-constant metrics
#'
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
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
#' @param bounds (optional) if TRUE, specifies whether the upper and  lower
#' bound should be calculated
#' if initvec is not specified, the function provides metrics in their upper and
#' lower bound, calculated based on the stage-biased vector
#' if initvec is specified, the function provides also the metrics calculated
#' based on the inital vector
#' @param initvec a list of numeric vectors or one-column matrices describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific', stage age structure
#' metric
#' @param ssd default as FALSE, if TRUE will calculate the demographic resilience
#' metrics based on the stable stage distribution vectors extracted from each matrix
#' @param popname a character string describing the name of the population
#' @param time a character string: "constant", "varying" or "both"
#'            "constant" : if the metrics are to be calculated over the whole
#'            study period
#'            "varying": if the metrics are to be calculated for each time step
#' @examples
#' \dontrun{
#'
#' #load packages
#' library(Rcompadre)
#' library(dplyr)
#' library(popdemo)
#'
#' # load data
#' comadre <- cdb_fetch("comadre")
#'
#' #selecting the blue crane
#' blue_crane <- comadre %>% dplyr::filter(SpeciesAccepted  == "Anthropoides paradiseus")
#'
#' #extracting matrices
#' blue_crane_matA <- Rcompadre::matA(blue_crane)
#'
#' # simulate an initial vector
#' Cranevec1 <- runif(5)
#' Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vec to sum to 1
#'
#'
#' BC_TVTC_demres <-
#'   demres(
#'     blue_crane_matA,
#'     metrics = "all",
#'     bounds = TRUE,
#'     initvec = Cranevec1,
#'     popname = "blue crane",
#'     time = "both"
#'   )
#'
#' }
#' @return A dataframe containing all the resilience metrics
#' @export
#' @name demres

demres <- function(listA,
                   metrics,
                   bounds = FALSE,
                   initvec = "n",
                   ssd = FALSE,
                   popname = NULL,
                   time) {

  if(!is.list(listA)) {
    stop("Warning: a list of matrices should be provided")
  }


  else{
    if(is.list(initvec)){
      if(!length(initvec) == length(listA)){
        stop("please provide a list of initial vectors with an equal length as the list of matrices")
      }
      if(time == "both") {
        temp_list <-
          mapply(function(A,X) {
            calc_resilience(A,
                            metrics = metrics,
                            bounds = bounds,
                            initvec = X,
                            popname = popname)
          }, A = listA, X = initvec)


        metres <- t(temp_list)
        colnames(metres)[-1] <- paste0(colnames(metres)[-1], "_TV")
        metres <- cbind(timestep = c(1:nrow(metres)), metres)

        meanA <- apply(simplify2array(listA), 1:2, mean)
        meanvec <- apply(simplify2array(initvec), 1, mean)
        res <- calc_resilience(meanA, metrics, bounds, meanvec, popname)
        names(res)[-1] <- paste0(names(res)[-1], "_TC")
        met <- cbind(metres, res)

        if(length(which(duplicated(names(met)))>0)) {
          met <- met[,-which(duplicated(names(met)))]
        }
        else{met <- met}
      }
      else {
        if (time == "varying"){
          temp_list <-
            mapply(function(A,X) {
              calc_resilience(A,
                              metrics = metrics,
                              bounds = bounds,
                              initvec = X,
                              popname = popname)
            }, A = listA, X = initvec)


          met <- t(temp_list)
          colnames(met)[-1] <- paste0(colnames(met)[-1], "_TV")
          met <- cbind(timestep = c(1:nrow(met)), met)



        }
        if(time == "constant") {
          meanA <- apply(simplify2array(listA), 1:2, mean)
          meanvec <- apply(simplify2array(initvec), 1, mean)
          res <- calc_resilience(meanA, metrics, bounds, meanvec, popname)
          names(res)[-1] <- paste0(names(res)[-1], "_TC")
          met <- res
        }
      }



    }
      else{
        if(time == "both") {
          temp_list <-
            lapply(
              listA,
              calc_resilience,
              metrics = metrics,
              bounds = bounds,
              initvec = initvec,
              popname = popname
            )
          metres <- do.call(rbind.data.frame, temp_list)
          names(metres)[-1] <- paste0(names(metres)[-1], "_TV")
          metres <- cbind(timestep = c(1:nrow(metres)), metres)

          meanA <- apply(simplify2array(listA), 1:2, mean)
          res <- calc_resilience(meanA, metrics, bounds, initvec, popname)
          names(res)[-1] <- paste0(names(res)[-1], "_TC")
          met <- cbind(metres, res)

          if(length(which(duplicated(names(met)))>0)) {
            met <- met[,-which(duplicated(names(met)))]
          }
          else{met <- met}
        }
        else {
          if(time == "varying"){
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
            names(met)[-1] <- paste0(names(met)[-1], "_TV")
            met <- cbind( timestep = c(1:nrow(met)), met)


          }
          if(time == "constant") {
            meanA <- apply(simplify2array(listA), 1:2, mean)
            res <- calc_resilience(meanA, metrics, bounds, initvec, popname)
            names(res)[-1] <- paste0(names(res)[-1], "_TC")
            met <- res
          }
        }
      }
    }
    return(met)
}
