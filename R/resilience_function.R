#' Provides time-varying and time-constant resilience metrics for
#' populations based on a list of matrix population models
#'
#' `resilience` calculates resilience metrics of a population based
#' on a list of matrix population models
#'
#' This function works with a list of matrices or just one matrix
#' and returns either time-varying metrics or time-constant metrics, depending
#' on what is requested by the user (option `time`).
#'
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
#' @param metrics "reac": Calculates reactivity: first-timestep amplification
#'                 and first-timestep attenuation for a matrix
#'                 projection model. \cr
#'                 "inertia": Calculates population inertia for a
#'                 matrix projection model.\cr
#'                 "dr": Calculates the damping ratio of a given
#'                 matrix projection model.\cr
#'                 "maxamp": Calculates maximal amplification for a
#'                 matrix projection model.\cr
#'                 "maxatt": Calculates maximal attenuation for a
#'                 matrix projection model.\cr
#'                 "convt": Calculates the time to convergence of a
#'                 matrix projection model.\cr
#'                 "all": all of the above metrics are provided.
#' @param bounds (optional) Boolean. Set to FALSE as default. If TRUE, specifies whether the upper and  lower
#' bound should be calculated. If initial vector is not specified, the function
#' provides metrics at their upper and lower bounds, calculated based on the stage-biased vector.
#' If vector is specified, the function provides also the metrics calculated
#' based on the initial vector.
#' @param vector a list of numeric vectors or one-column matrices describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific' resilience metric,
#' based on the stage or age-structure.
#' @param TDvector Boolean. Set to FALSE as default. Specifies whether or not the
#' user wants to obtain a time-dependent list of initial vectors. This vector
#' corresponds to the population stage distribution that is obtained from the projection
#' of the population to the current time step using the specified matrix for each time step.
#' @param popname a character string describing the name of the population.
#' @param time set to "both" as default. A character string: "constant", "varying" or "both" \cr
#'            "constant": if the metrics are to be calculated over the whole study period; \cr
#'            "varying": if the metrics are to be calculated for each time step.
#' @param accuracy the accuracy with which to determine convergence to asymptotic growth,
#' expressed as a proportion. Set to 0.01 by default.
#' @param f A character specifying whether the output should be shown in
#' "long" (demographic resilience metrics as row names) or in "wide" (demographic
#' resilience metrics as column names) format. Defaults to "wide".
#' @param iterations the maximum number of iterations of the model before the code breaks. For slowly-converging models
#' and/or high specified convergence accuracy, this may need to be increased.
#' Set to 1e+05 by default.
#' **Vik**: the option above (iterations) is used for convergence time only, right? Should be mentioned then
#' @param verbose Boolean. Set to TRUE as default. Indicates whether the messages about failure
#' to compute particular metric should be displayed or not (default = TRUE)
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
#'     TDvector = FALSE,
#'     popname = "blue crane",
#'     time = "varying",
#'     verbose = TRUE
#'   )
#'
#' @return An object of class "resil", which is a dataframe
#' containing the requested resilience metrics.
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
                   iterations = 1e+05,
                   f = 'wide') {

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

          n.obs <- sapply(message_varying_temp, length)
          seq.max <- seq_len(max(n.obs))
          if(length(seq.max)>0){
          message_varying <- data.frame(sapply(message_varying_temp, "[", i = seq.max))
          message_varying[is.na(message_varying)] <- ""
          colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq (1:length(listA)))
        }

        metres <- do.call("rbind", temp_list)

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

          n.obs <- sapply(message_varying_temp, length)
          seq.max <- seq_len(max(n.obs))
          if(length(seq.max)>0){

            message_varying <- data.frame(sapply(message_varying_temp, "[", i = seq.max))
            message_varying[is.na(message_varying)] <- ""
            colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq (1:length(listA)))
          }

          met <- do.call("rbind", temp_list)
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
#' @param object an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @return summary statistics
#' @export
#'
summary.resil <- function(object, f = 'wide', ...) {
  demres_dist(object, f)
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


