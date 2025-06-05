#' Calculates time-varying resilience metrics for
#' populations based on a list of matrix population models
#'
#' This function works with a list of matrices or just with one matrix
#' and returns time-varying metrics.
#'
#' @inheritParams calc_resilience
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
#' @param vector a list of numeric vectors or one-column matrices describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific' resilience metric,
#' based on the stage- or age-structure.
#' @param TDvector Boolean. Set to FALSE as default. Specifies whether or not the
#' user wants to obtain a time-dependent list of initial vectors. This vector
#' corresponds to the population stage distribution that is obtained from the projection
#' of the population to the current time step using the specified matrix for each time step.
#' @param f A character specifying whether the output should be shown in
#' "long" (demographic resilience metrics as row names) or in "wide" (demographic
#' resilience metrics as column names) format. Defaults to "wide".
#' @examples
#'
#' # load data
#' data(adeliepenguin)
#' penguinvec1 <- c(25, 75)
#'
#' # simulate an initial vector
#' #set.seed(1234)
#' # subset of the list to check how it works with a list of pop vectors
#' adeliesubs <- adeliepenguin[1:10]
#' penguinvec_list_rel <- replicate(n =10, expr= runif(2))
#' penguinvec_list_norm <- lapply(1:10, FUN = function(x){penguinvec_list_rel[, x]/ sum(penguinvec_list_rel[, x])})
#' penguinvec_list <- lapply(penguinvec_list_norm, FUN = function(x){round(x*100)})
#'
#' AP_TVTC_demres <-
#'   resilience(
#'     listA = adeliesubs,
#'     metrics = "all",
#'     vector = penguinvec_list,
#'     TDvector = FALSE,
#'     popname = "adelie penguin",
#'     verbose = TRUE,
#'     return.N = TRUE,
#'     return.t = TRUE
#'   )  ## TDvector function still to be fully debugged
#'
#'# test with one matrix
#' adelie <- adeliepenguin[[1]]
#'
#' AP_demres_OneMat <-
#'   resilience(
#'     listA = adelie,
#'     metrics = "all",
#'     vector = penguinvec1,
#'     TDvector = FALSE,
#'     popname = "adelie penguin",
#'     verbose = TRUE,
#'     return.N = TRUE,
#'     return.t = TRUE
#'   )  ## TDvector function still to be fully debugged
#'
#' @return An object of class "resil", which is a dataframe
#' containing the requested resilience metrics.
#' @export
#' @name resilience

resilience <- function(listA,
                       metrics = "all",
                       # bounds = FALSE,
                       vector,
                       TDvector = FALSE, ## V: this has to be fixed yet, returns all standardised
                       popname = NULL,
                       # time = "both",
                       verbose = TRUE,
                       accuracy = 0.01,
                       iterations = 1e+05,
                       f = 'wide',
                       return.N = TRUE,
                       return.t = TRUE) {
  message_varying <- character(0)

  if (is.list(listA) && length(listA) == 1) {
    warning(
      "You provided a list of one matrix.
    A list of several matrices should be provided.
    Resilience is nevertheless calculated for this one matrix"
    )
    listA <- listA[[1]]
  }

  if (!is.list(listA)) {
    warning(
      "A list of several matrices should be provided.
            Resilience is nevertheless calculated for this one matrix"
    )
    met <- calc_resilience(
      A = listA,
      metrics = metrics,
      # bounds = bounds,
      vector = vector,
      popname = popname,
      verbose = verbose,
      accuracy = accuracy,
      iterations = iterations,
      return.N = return.N,
      return.t = return.t
    )

    message <- data.frame(t(attr(met, "msg")))
    rownames(message) <- NULL
    colnames(message) <- "Message for one matrix"


    if (verbose) {
      if (length(message) > 0) {
        print(message)
      }
    }
  }

  else{
    if (TDvector) {
      vector <- get_TD_vector(IV = vector[[1]], listA = listA)
    }

    if (is.list(vector)) {
      if (!length(vector) == length(listA)) {
        stop(
          "please provide a list of initial vectors with an equal length as the list of matrices"
        )
      }
      temp_list <-
        mapply(function(A, X) {
          calc_resilience(
            A,
            metrics = metrics,
            # bounds = bounds,
            vector = X,
            popname = popname,
            verbose = verbose,
            accuracy = accuracy,
            iterations = iterations,
            return.N = return.N,
            return.t = return.t
          )
        },
        A = listA,
        X = vector,
        SIMPLIFY = FALSE)

      message_varying_temp <- lapply(temp_list, function(e)
        attr(e, "msg"))

      n.obs <- sapply(message_varying_temp, length)
      seq.max <- seq_len(max(n.obs))
      if (length(seq.max) > 0) {
        #message_varying <- data.frame(unlist(message_varying_temp)) #- for the TDvec - this does not work properly, drops NAs
        message_varying <- data.frame(sapply(message_varying_temp, "[", i = seq.max))
        message_varying[is.na(message_varying)] <- ""
        colnames(message_varying) <- NULL
        rownames(message_varying) <- paste0("Message for resilience calculated at time step ",
                                            seq_len(length(listA)))
      }

      metres <- do.call("rbind", temp_list)

      # colnames(metres)[-1] <- paste0(colnames(metres)[-1], "_TV")
      metres <- cbind(timestep = c(seq_len(nrow(metres))), metres)

      # meanA <- apply(simplify2array(listA), 1:2, mean)
      # meanvec <- apply(simplify2array(vector), 1, mean)
      # res <- calc_resilience(A = meanA,
      #                        metrics = metrics,
      #                        bounds = bounds,
      #                        vector = meanvec,
      #                        popname = popname,
      #                        verbose = verbose,
      #                        accuracy = accuracy,
      #                        iterations = iterations)
      #
      #  if(!is.null(attr(res, "msg"))){
      #   message_constant <- data.frame(t(attr(res, "msg")))
      #   if(length(message_constant) > 0){
      #     rownames(message_constant) <- NULL
      #     colnames(message_constant) <- "Message for time-constant resilience"
      # }
      # }

      # names(res)[-1] <- paste0(names(res)[-1], "_TC")
      # met <- cbind(metres, res)

      if (length(which(duplicated(names(metres)))) > 0) {
        metres <- metres[, -which(duplicated(names(metres)))]
      }
      else{
        metres <- metres
      }

      # else {
      #   if (time == "varying"){
      #     temp_list <-
      #       mapply(function(A,X) {
      #         calc_resilience(A,
      #                         metrics = metrics,
      #                         bounds = bounds,
      #                         vector = X,
      #                         popname = popname,
      #                         verbose = verbose,
      #                         accuracy = accuracy,
      #                         iterations = iterations)
      #       }, A = listA, X = vector, SIMPLIFY = FALSE)
      #
      #     message_varying_temp <- sapply(temp_list, function(e) attr(e, "msg"))
      #
      #     n.obs <- sapply(message_varying_temp, length)
      #     seq.max <- seq_len(max(n.obs))
      #     if(length(seq.max) > 0){
      #
      #       message_varying <- data.frame(sapply(message_varying_temp, "[", i = seq.max))
      #       message_varying[is.na(message_varying)] <- ""
      #       colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq_len(length(listA)))
      #     }
      #
      #     met <- do.call("rbind", temp_list)
      #     colnames(met)[-1] <- paste0(colnames(met)[-1], "_TV")
      #     met <- cbind(timestep = c(seq_len(nrow(met))), met)
      #
      #   }
      #   if(time == "constant") {
      #     meanA <- apply(simplify2array(listA), 1:2, mean)
      #     meanvec <- apply(simplify2array(vector), 1, mean)
      #     res <- calc_resilience(A = meanA,
      #                            metrics = metrics,
      #                            bounds = bounds,
      #                            vector = vector,
      #                            popname = popname,
      #                            verbose = verbose,
      #                            accuracy = accuracy,
      #                            iterations = iterations)
      #
      #     if(!is.null(attr(res, "msg"))){
      #       message_constant <- data.frame(t(attr(res, "msg")))
      #       if(length(message_constant) > 0){
      #         rownames(message_constant) <- NULL
      #         colnames(message_constant) <- "Message for time-constant resilience"
      #       }
      #     }
      #
      #       names(res)[-1] <- paste0(names(res)[-1], "_TC")
      #       met <- res
      #     }
      #   }
      #
    }

    else{
      temp_list <-
        lapply(
          listA,
          calc_resilience,
          metrics,
          # bounds,
          vector,
          popname,
          verbose,
          accuracy,
          iterations,
          return.N,
          return.t
        )

      message_varying <- data.frame(sapply(temp_list, function(e)
        attr(e, "msg")))
      if (length(message_varying) > 0) {
        colnames(message_varying) <- NULL
        rownames(message_varying) <- paste0("Message for resilience calculated at time step ",
                                            seq_len(length(listA)))
      }
      metres <- do.call(rbind.data.frame, temp_list)
      # names(metres)[-1] <- paste0(names(metres)[-1], "_TV")
      metres <- cbind(timestep = c(seq_len(nrow(metres))), metres)

      # meanA <- apply(simplify2array(listA), 1:2, mean)
      #          res <- calc_resilience(A = meanA,
      #                                 metrics = metrics,
      #                                 bounds = bounds,
      #                                 vector = vector,
      #                                 popname = popname,
      #                                 verbose = verbose,
      #                                 accuracy = accuracy,
      #                                 iterations = iterations)
      #          if(!is.null(attr(res, "msg"))){
      # message_constant <- data.frame(t(attr(res, "msg")))
      # if(length(message_constant) > 0){
      # rownames(message_constant) <- NULL
      # colnames(message_constant) <- "Message for time-constant resilience"
      # }
      #          }
      #
      # names(res)[-1] <- paste0(names(res)[-1], "_TC")
      # met <- cbind(metres, res)

      if (length(which(duplicated(names(metres)))) > 0) {
        metres <- metres[, -which(duplicated(names(metres)))]
      }
      else{
        metres <- metres
      }

      # else {
      #   if(time == "varying"){
      #     temp_list <-
      #       lapply(
      #         listA,
      #         calc_resilience,
      #         metrics,
      #         bounds,
      #         vector,
      #         popname,
      #         verbose,
      #         accuracy,
      #         iterations
      #       )
      #
      #     message_varying <- data.frame(sapply(temp_list, function(e) attr(e, "msg")))
      #     if(length(message_varying) > 0){
      #     colnames(message_varying) <- paste0("Message for time-varying resilience at time step ", seq_len(length(listA)))
      #     }
      #     met <- do.call(rbind.data.frame, temp_list)
      #     names(met)[-1] <- paste0(names(met)[-1], "_TV")
      #     met <- cbind( timestep = c(seq_len(nrow(met))), met)
      #
      #
      #   }
      #   if(time == "constant") {
      #     meanA <- apply(simplify2array(listA), 1:2, mean)
      #     res <- calc_resilience(A = meanA,
      #                            metrics = metrics,
      #                            bounds = bounds,
      #                            vector = vector,
      #                            popname = popname,
      #                            verbose = verbose,
      #                            accuracy = accuracy,
      #                            iterations = iterations)
      #     if(!is.null(attr(res, "msg"))){
      #     message_constant <- data.frame(t(attr(res, "msg")))
      #     if(length(message_constant) > 0){
      #     rownames(message_constant) <- NULL
      #     colnames(message_constant) <- "Message for time-constant resilience"
      #     }
      #     }
      #     names(res)[-1] <- paste0(names(res)[-1], "_TC")
      #     met <- res
      #   }
      # }
    }

    if (verbose) {
      if (length(message_varying) > 0) {
        print(message_varying)
      }

      # if(length(message_constant) > 0){
      #   print(message_constant)
      # }
    }
  }

  class(metres) <- c("resil", class(metres))

  return(metres)
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
#' @param f A character specifying whether the output should be shown in
#' "long" (demographic resilience metrics as row names) or in "wide" (demographic
#' resilience metrics as column names) format. Defaults to "wide".
#' @param ... further arguments passed to or from other methods
#' @return summary statistics
#' @export
#' @seealso [demres_summary()] for details
#'
summary.resil <- function(object, f = 'wide', ...) {
  demres_summary(object, f)
}

#' Plotting method for objects of class resil
#'
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @return plots
#' @export
#' @seealso [demres_plot()] for details
#'
plot.resil <- function(x, ...) {
  demres_plot(x)
}
