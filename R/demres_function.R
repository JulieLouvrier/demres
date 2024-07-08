#' Provides time-varying and time-constant resilience metrics for animal
#' populations based on a list of matrix population models
#'
#' `demres` calculates resilience metrics of a population based
#' on a list of matrix population models
#'
#' This function applies the function "calc_resilience" to a list of matrices
#' and returns either time-varying metrics or time-constant metrics
#'
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
#' @param metrics "reac": Calculates reactivity: first-timestep amplification
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
#' if vector is not specified, the function provides metrics in their upper and
#' lower bound, calculated based on the stage-biased vector
#' if vector is specified, the function provides also the metrics calculated
#' based on the inital vector
#' @param vector a list of numeric vectors or one-column matrices describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific', stage age structure
#' metric
#' @param TDvector specifies whether or not the user wants to get a Time-Dependant list of initial vectors,
#' corresponding to the population stage distribution projection for each year with each matrix of each year.
#' The result is a list of X initial vectors with X the number of matrices that are in the list.
#' @param popname a character string describing the name of the population
#' @param time a character string: "constant", "varying" or "both"
#'            "constant" : if the metrics are to be calculated over the whole
#'            study period
#'            "varying": if the metrics are to be calculated for each time step
#' @examples
#'
#' # load data
#' # load data
#' data(bluecrane)
#'
#' # simulate an initial vector
#' Cranevec1 <- runif(5)
#' Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vec to sum to 1
#'
#'
#' BC_TVTC_demres <-
#'   demres(
#'     bluecrane,
#'     metrics = "all",
#'     bounds = TRUE,
#'     vector = Cranevec1,
#'     TDvector = TRUE,
#'     popname = "blue crane",
#'     time = "both"
#'   )
#'
#' @return A dataframe containing all the resilience metrics
#' @export
#' @name demres

demres <- function(listA,
                   metrics,
                   bounds = FALSE,
                   vector = "n",
                   TDvector = FALSE,
                   popname = NULL,
                   time) {

  if(!is.list(listA)) {
    stop("Warning: a list of matrices should be provided")
  }

  else{
    if(TDvector == TRUE){
      vector <- get_TD_vector(IV = vector, listA = listA)
    }

    if(is.list(vector)){
      if(!length(vector) == length(listA)){
        stop("please provide a list of initial vectors with an equal length as the list of matrices")
      }
      if(time == "both") {
        temp_list <-
          mapply(function(A,X) {
            calc_resilience(A,
                            metrics = metrics,
                            bounds = bounds,
                            vector = X,
                            popname = popname,
                            verbose = FALSE)
          }, A = listA, X = vector)

        metres <- t(temp_list)
        colnames(metres)[-1] <- paste0(colnames(metres)[-1], "_TV")
        metres <- cbind(timestep = c(1:nrow(metres)), metres)

        meanA <- apply(simplify2array(listA), 1:2, mean)
        meanvec <- apply(simplify2array(vector), 1, mean)
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
                              vector = X,
                              popname = popname)
            }, A = listA, X = vector)


          met <- t(temp_list)
          colnames(met)[-1] <- paste0(colnames(met)[-1], "_TV")
          met <- cbind(timestep = c(1:nrow(met)), met)



        }
        if(time == "constant") {
          meanA <- apply(simplify2array(listA), 1:2, mean)
          meanvec <- apply(simplify2array(vector), 1, mean)
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
              vector = vector,
              popname = popname
            )

          msg <- character(0)
          msg <- unique(sapply(temp_list, function(e) attr(e, "msg")))

          metres <- do.call(rbind.data.frame, temp_list)
          names(metres)[-1] <- paste0(names(metres)[-1], "_TV")
          metres <- cbind(timestep = c(1:nrow(metres)), metres)

          meanA <- apply(simplify2array(listA), 1:2, mean)
          res <- calc_resilience(meanA, metrics, bounds, vector, popname)
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
                vector = vector,
                popname = popname
              )
            met <- do.call(rbind.data.frame, temp_list)
            names(met)[-1] <- paste0(names(met)[-1], "_TV")
            met <- cbind( timestep = c(1:nrow(met)), met)


          }
          if(time == "constant") {
            meanA <- apply(simplify2array(listA), 1:2, mean)
            res <- calc_resilience(meanA, metrics, bounds, vector, popname)
            names(res)[-1] <- paste0(names(res)[-1], "_TC")
            met <- res
          }
        }
      }
  }

    # if (length(msg) > 0) {
    #   message(msg)
    # }

    class(met) <- c("resil", class(met))

    return(met)
}

#' Printing method for objects of class resil
#'
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#'
#' @return the argument `x` (invisibly)
#' @export
#'
print.resil <- function(x, ...) {
  class(x) <- "data.frame"
  print(x, ...)
}

#' Summary method for objects of class resil
#'
#' @param object an object used to select a method
#' @param ... further arguments passed to or from other methods
#'
#' @return summary statistics
#' @export
#'
summary.resil <- function(object, ...) {
  demres_dist(object, metric = "inertia", measure = "all", ...)
}

#' Plotting method for objects of class resil
#'
#' @inheritParams demres_plot
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#'
#' @return the argument `x` (invisibly)
#' @export
#'
plot.resil <- function(x, rRMSE = FALSE,
                          RMSE = FALSE,
                          MAPE = FALSE, ...) {
  demres_plot(x, metric = "inertia", rRMSE = rRMSE, RMSE = RMSE, MAPE = MAPE)
}


