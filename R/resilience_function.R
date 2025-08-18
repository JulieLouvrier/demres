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
#' user wants to use, instead of specified population vectors, so-called "time-dependent" vectors that
#' are obtained from the projection of the population to the current time step using
#'  the specified matrix for each time step.
#' @examples
#' # load data
#' data(adeliepenguin)
#'
#' # simulate an initial vector
#' set.seed(125435)
#' vec <- round(runif(2) * 100 )
#' vector_list <- lapply(1:28, function(i) vec)
#'
#' AP_demres <-
#'   resilience(
#'     listA = adeliepenguin,
#'     metrics = "all",
#'     vector = vector_list,
#'     TDvector = FALSE,
#'     popname = "adelie penguin",
#'     verbose = TRUE,
#'     return.N = TRUE,
#'     return.t = TRUE,
#'     target.N = 500
#'   )
#'
#' AP_demres1 <-
#'   resilience(
#'     listA = list(adeliepenguin[[1]]),
#'     metrics = "all",
#'     vector = list(vector_list[[1]]),
#'     TDvector = FALSE,
#'     popname = "adelie penguin",
#'     verbose = TRUE,
#'     return.N = TRUE,
#'     return.t = TRUE
#'   )
#'
#' # an example with time-dependent population vector
#' AP_demres_TD <-
#'   resilience(
#'     listA = adeliepenguin,
#'     metrics = "all",
#'     vector = vector_list,
#'     TDvector = TRUE,
#'     popname = "adelie penguin",
#'     verbose = TRUE,
#'     return.N = TRUE,
#'     return.t = TRUE
#'   )
#'
#' @return An object of class "resil", which is a dataframe
#' containing the requested resilience metrics.
#' @export
#' @name resilience

resilience <- function(listA,
                       metrics = "all",
                       vector,
                       TDvector = FALSE,
                       popname = NULL,
                       verbose = TRUE,
                       accuracy = 0.01,
                       iterations = 1e+05,
                       return.N = TRUE,
                       return.t = TRUE,
                       target.N = NULL) {
  message_varying <- character(0)

  if (!is.list(vector)) {
    stop("The population vector should be a list of one or several vectors")
  }

  if (is.list(listA) && length(listA) == 1) {
    warning(
      "You provided a list of one matrix.
    A list of several matrices should be provided.
    Resilience is nevertheless calculated for this one matrix"
    )
    listA <- listA[[1]]
    vector <- vector[[1]]
  }

  if (!is.list(listA)) {
    warning(
      "A list of several matrices should be provided.
            Resilience is nevertheless calculated for this one matrix"
    )
    metres <- calc_resilience(
      A = listA,
      metrics = metrics,
      vector = vector,
      popname = popname,
      verbose = verbose,
      accuracy = accuracy,
      iterations = iterations,
      return.N = return.N,
      return.t = return.t,
      target.N = target.N
    )

    message <- data.frame(t(attr(metres, "msg")))
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
            vector = X,
            popname = popname,
            verbose = verbose,
            accuracy = accuracy,
            iterations = iterations,
            return.N = return.N,
            return.t = return.t,
            target.N = target.N
          )
        },
        A = listA,
        X = vector,
        SIMPLIFY = FALSE)

      message_varying_temp <- lapply(temp_list, function(e)
        attr(e, "msg"))

      n.obs <- sapply(message_varying_temp, length)
      message_varying_temp <- lapply(message_varying_temp, FUN = function(x){ifelse(is.null(x), x[1] <- NA, x[1] <- x[1])} )
      seq.max <- seq_len(max(n.obs))
      if (length(seq.max) > 0) {
        message_varying <- data.frame(sapply(message_varying_temp, "[", i = seq.max))
        message_varying[is.na(message_varying)] <- ""
        colnames(message_varying) <- NULL
        rownames(message_varying) <- paste0("Message for resilience calculated at time step ",
                                            seq_len(length(listA)))
        message_varying <- message_varying[!message_varying[, 1] == "", ]
      }

      metres <- do.call("rbind", temp_list)

      metres <- cbind(timestep = c(seq_len(nrow(metres))), metres)

      if (length(which(duplicated(names(metres)))) > 0) {
        metres <- metres[, -which(duplicated(names(metres)))]
      }
      else{
        metres <- metres
      }
    }

    if (verbose) {
      if (length(message_varying) > 0) {
        print(message_varying)
      }
    }
  }

  if(return.t == FALSE) {
    if ('maxatt' %in% metrics) {
      colnames_remove <- paste0(colnames(metres)[colnames(metres) %in% c('maxatt')], '.t')
      metres <- metres[, setdiff(colnames(AP_demres), col_rem)]
    }
    if ('maxamp' %in% metrics) {
      colnames_remove <- paste0(colnames(metres)[colnames(metres) %in% c('maxamp')], '.t')
      metres <- metres[, setdiff(colnames(AP_demres), col_rem)]
    }
    if ('reac' %in% metrics) {
      colnames_remove <- paste0(colnames(metres)[colnames(metres) %in% c('reac')], '.t')
      metres <- metres[, setdiff(colnames(AP_demres), col_rem)]
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
#' @param ... further arguments passed to or from other methods
#' @return summary statistics
#' @export
#' @seealso [demres_summary()] for details
#'
summary.resil <- function(object, ...) {
  demres_summary(object, ... )
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
  demres_plot(x, ...)
}
