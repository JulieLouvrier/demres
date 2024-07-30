#' Provides time-varying and time-constant resilience metrics for animal
#' populations based on a list of matrix population models
#'
#' `resilience` calculates resilience metrics of a population based
#' on a list of matrix population models
#'
#' This function works with to a list of matrices or just one matrix
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
#'                 "convt": Calculate the time to convergence of a population
#'                 matrix projection model from the model projection
#'                 "all": all of the above metrics are provided
#' @param bounds (optional) set to FALSE as default. If TRUE, specifies whether the upper and  lower
#' bound should be calculated
#' if vector is not specified, the function provides metrics in their upper and
#' lower bound, calculated based on the stage-biased vector
#' if vector is specified, the function provides also the metrics calculated
#' based on the inital vector
#' @param vector a list of numeric vectors or one-column matrices describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific', stage age structure
#' metric
#' @param TDvector set to FALSE as default. Specifies whether or not the user wants to get a Time-Dependant list of initial vectors,
#' corresponding to the population stage distribution projection for each year with each matrix of each year.
#' @param popname a character string describing the name of the population
#' @param time set to "both" as default. A character string: "constant", "varying" or "both"
#'            "constant" : if the metrics are to be calculated over the whole
#'            study period
#'            "varying": if the metrics are to be calculated for each time step
#' @param accuracy the accuracy with which to determine convergence on asymptotic growth,
#' expressed as a proportion. Set to 0.01 by default.
#' @param iterations the maximum number of iterations of the model before the code breaks. For slowly-converging models
#' and/or high specified convergence accuracy, this may need to be increased.
#' Set to 1e+05 by default.
#' @param verbose set to TRUE as default a boolean indicating if messages about failure to compute particular metrics should be displayed or not (default = TRUE)
#' @examples
#'
#' # load data
#' data(bluecrane)
#'
#' # simulate an initial vector
#' set.seed(1234)
#' Cranevec1 <- runif(5)
#' Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vec to sum to 1
#'
#'
#' BC_TVTC_demres <-
#'   resilience(
#'     listA = bluecrane,
#'     metrics = "all",
#'     bounds = TRUE,
#'     vector = Cranevec1,
#'     TDvector = TRUE,
#'     popname = "blue crane",
#'     time = "both",
#'     verbose = TRUE
#'   )
#'
#' @return A dataframe containing all the resilience metrics
#' @export
#' @name resilience

resilience <- function(listA,
                   metrics = "all",
                   bounds = FALSE,
                   vector = "n",
                   TDvector = FALSE,
                   popname = NULL,
                   time = "both",
                   verbose = TRUE,
                   accuracy = 0.01,
                   iterations = 1e+05) {

  message_varying <- character(0)
  message_constant <- character(0)

  if(is.list(listA) && length(listA) == 1){
    message("Warning: you provided a list of one matrix.
    A list of several matrices should be provided.
    Resilience is nevertheless calculated for this one matrix")
    listA <- listA[[1]]
  }

  if(!is.list(listA)) {
    message("Warning: a list of several matrices should be provided.
            Resilience is nevertheless calculated for this one matrix")
    met <- calc_resilience(A = listA,
                           metrics = metrics,
                           bounds = bounds,
                           vector = vector,
                           popname = popname,
                           verbose = verbose,
                           accuracy = accuracy,
                           iterations = iterations)

    message <- data.frame(t(attr(met, "msg")))
    rownames(message) <- NULL
    colnames(message) <- "Message for one matrix"


    if (verbose) {
      if(length(message) > 0){
        print(message)
      }
    }
  }

  else{
  if(TDvector){
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
                            verbose = verbose,
                            accuracy = accuracy,
                            iterations = iterations)
          }, A = listA, X = vector, SIMPLIFY = FALSE)

        message_varying_temp <- sapply(temp_list, function(e) attr(e, "msg"))
        if(length(message_varying_temp)>0){

          n.obs <- sapply(message_varying_temp, length)
          seq.max <- seq_len(max(n.obs))
          message_varying <- data.frame(sapply(message_varying_temp, "[", i = seq.max))
          message_varying[is.na(message_varying)] <- ""
          colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq (1:length(listA)))
        }

        # n.obs.res <- sapply(temp_list, length)
        # seq.max.res <- seq_len(max(n.obs.res))
        # metres.temp <- data.frame(sapply(temp_list, "[", i = seq.max.res))
        metres <- do.call("rbind", temp_list)
        # rownames(metres) <- seq(1:length(listA))

        colnames(metres)[-1] <- paste0(colnames(metres)[-1], "_TV")
        metres <- cbind(timestep = c(1:nrow(metres)), metres)

        meanA <- apply(simplify2array(listA), 1:2, mean)
        meanvec <- apply(simplify2array(vector), 1, mean)
        res <- calc_resilience(A = meanA,
                               metrics = metrics,
                               bounds = bounds,
                               vector = meanvec,
                               popname = popname,
                               verbose = verbose,
                               accuracy = accuracy,
                               iterations = iterations)

         if(!is.null(attr(res, "msg"))){
          message_constant <- data.frame(t(attr(res, "msg")))
          if(length(message_constant)>0){
            rownames(message_constant) <- NULL
            colnames(message_constant) <- "Message for time-constant resilience"
          }
        }

        names(res)[-1] <- paste0(names(res)[-1], "_TC")
        met <- cbind(metres, res)

        if(length(which(duplicated(names(met))))>0) {
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
                              popname = popname,
                              verbose = verbose,
                              accuracy = accuracy,
                              iterations = iterations)
            }, A = listA, X = vector, SIMPLIFY = FALSE)

          message_varying_temp <- sapply(temp_list, function(e) attr(e, "msg"))
          if(length(message_varying_temp)>0){

            n.obs <- sapply(message_varying_temp, length)
            seq.max <- seq_len(max(n.obs))
            message_varying <- data.frame(sapply(message_varying_temp, "[", i = seq.max))
            message_varying[is.na(message_varying)] <- ""
            colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq (1:length(listA)))
          }

          # n.obs.res <- sapply(temp_list, length)
          # seq.max.res <- seq_len(max(n.obs.res))
          # metres.temp <- data.frame(sapply(temp_list, "[", i = seq.max.res))
          met <- do.call("rbind", temp_list)
          # rownames(metres) <- seq(1:length(listA))
          colnames(met)[-1] <- paste0(colnames(met)[-1], "_TV")
          met <- cbind(timestep = c(1:nrow(met)), met)

        }
        if(time == "constant") {
          meanA <- apply(simplify2array(listA), 1:2, mean)
          meanvec <- apply(simplify2array(vector), 1, mean)
          res <- calc_resilience(A = meanA,
                                 metrics = metrics,
                                 bounds = bounds,
                                 vector = vector,
                                 popname = popname,
                                 verbose = verbose,
                                 accuracy = accuracy,
                                 iterations = iterations)

          if(!is.null(attr(res, "msg"))){
            message_constant <- data.frame(t(attr(res, "msg")))
            if(length(message_constant)>0){
              rownames(message_constant) <- NULL
              colnames(message_constant) <- "Message for time-constant resilience"
            }
          }

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
            metrics,
            bounds,
            vector,
            popname,
            verbose,
            accuracy,
            iterations
          )

        message_varying <- data.frame(sapply(temp_list, function(e) attr(e, "msg")))
        if(length(message_varying)>0){
        colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq (1:length(listA)))
        }
        metres <- do.call(rbind.data.frame, temp_list)
        names(metres)[-1] <- paste0(names(metres)[-1], "_TV")
        metres <- cbind(timestep = c(1:nrow(metres)), metres)

        meanA <- apply(simplify2array(listA), 1:2, mean)
                 res <- calc_resilience(A = meanA,
                                        metrics = metrics,
                                        bounds = bounds,
                                        vector = vector,
                                        popname = popname,
                                        verbose = verbose,
                                        accuracy = accuracy,
                                        iterations = iterations)
                 if(!is.null(attr(res, "msg"))){
        message_constant <- data.frame(t(attr(res, "msg")))
        if(length(message_constant)>0){
        rownames(message_constant) <- NULL
        colnames(message_constant) <- "Message for time-constant resilience"
        }
                 }

        names(res)[-1] <- paste0(names(res)[-1], "_TC")
        met <- cbind(metres, res)

        if(length(which(duplicated(names(met))))>0) {
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
              metrics,
              bounds,
              vector,
              popname,
              verbose,
              accuracy,
              iterations
            )

          message_varying <- data.frame(sapply(temp_list, function(e) attr(e, "msg")))
          if(length(message_varying)>0){
          colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq (1:length(listA)))
          }
          met <- do.call(rbind.data.frame, temp_list)
          names(met)[-1] <- paste0(names(met)[-1], "_TV")
          met <- cbind( timestep = c(1:nrow(met)), met)


        }
        if(time == "constant") {
          meanA <- apply(simplify2array(listA), 1:2, mean)
          res <- calc_resilience(A = meanA,
                                 metrics = metrics,
                                 bounds = bounds,
                                 vector = vector,
                                 popname = popname,
                                 verbose = verbose,
                                 accuracy = accuracy,
                                 iterations = iterations)
          if(!is.null(attr(res, "msg"))){
          message_constant <- data.frame(t(attr(res, "msg")))
          if(length(message_constant)>0){
          rownames(message_constant) <- NULL
          colnames(message_constant) <- "Message for time-constant resilience"
          }
          }
          names(res)[-1] <- paste0(names(res)[-1], "_TC")
          met <- res
        }
      }
    }

  if (verbose) {
    if(length(message_varying) > 0){
      print(message_varying)
    }

  if(length(message_constant) > 0){
    print(message_constant)
  }
    else{
      print("no error message occurred")
    }
  }
}

  class(met) <- c("resil", class(met))

  return(met)
}

#' Printing method for objects of class resil
#'
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @return the argument `x` (invisibly)
#' @export
#'
print.resil <- function(x, ...) {
  class(x) <- "data.frame"
  print(x, ...)
}

#' Summary method for objects of class resil
#'
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @return summary statistics
#' @export
#'
summary.resil <- function(x, ...) {
  demres_dist(x)
}

#' Plotting method for objects of class resil
#'
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @return plots
#' @export
#'
plot.resil <- function(x,...) {
  demres_plot(x)
}


