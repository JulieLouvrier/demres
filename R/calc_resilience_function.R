#' Provides resilience metrics from the package _{popdemo}_ for one matrix
#'
#' `calc_resilience` calculates resilience metrics of a population based
#' on a matrix population model
#'
#' This function computes different metrics of resilience, given by the _{popdemo}_
#' package based on a matrix projection model.
#'
#'
#' @param A a square, primitive, irreducible, non-negative numeric matrix of any
#' dimension
#' @param metrics "convt": Calculates the time to convergence of a
#'                 matrix projection model.\cr
#'                 "dr": Calculates the damping ratio of a given
#'                 matrix projection model.\cr
#'                 "maxamp": Calculates maximal amplification for a
#'                 matrix projection model.\cr
#'                 "maxatt": Calculates maximal attenuation for a
#'                 matrix projection model.\cr
#'                 "reac": Calculates reactivity: first time step amplification
#'                 and first time step attenuation for a matrix
#'                 projection model.\cr
#'                 "tt": Time to Target: if return.N = TRUE, calculates the
#'                 number of timesteps necessary to reach a target population
#'                 abundance. \cr
#'                 "all": all of the above metrics are provided.
#' @param vector a numeric vector or one-column matrix describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific'
#' resilience metric
#' @param popname (optional) a character string describing the name of the population.
#' @param accuracy (optional) option for calculating convergence time: the accuracy with which to determine convergence to asymptotic growth,
#' expressed as a proportion. Set to 0.01 by default.
#' @param iterations (optional) option for calculating convergence time: the maximum number of iterations of the model. Set to 1e+05 by default. For slowly-converging models
#' and/or high specified convergence accuracy, this may need to be increased.
#' @param verbose (optional) Boolean. Set to TRUE as default. Indicates whether the messages about failure
#' to compute particular metric should be displayed or not (default = TRUE)
#' @param return.N (optional) Boolean. Set to TRUE as default. If TRUE returns population size.
#' If set to FALSE, returns the standardised value of the requested metric.
#' @param return.t (optional) Boolean. If TRUE, returns the time at which the metric is reached in the population projection
#' If FALSE does not return the time.
#' @param target.N (optional) Numeric. Specifies the population abundance target to calculate
#' the Time to Target metric
#' @examples
#' data(adeliepenguin)
#'
#' # simulate an initial vector
#' set.seed(125435)
#' penguinvec1 <- round(runif(2) * 100 )
#'
#' penguin1 <- adeliepenguin[[1]]
#'
#' all_penguin_demres <- calc_resilience(penguin1, metrics = c("all"),
#' vector = penguinvec1, popname = "adelie penguin", verbose = TRUE,
#' return.N = TRUE, return.t = FALSE, target.N = 500)
#'
#' @return A vector containing all the resilience metrics
#' @name calc_resilience
#' @keywords internal

calc_resilience <-
  function(A,
           metrics = "all",
           vector = vect,
           popname = NULL,
           verbose = TRUE,
           accuracy = 0.01,
           iterations = 1e+05,
           return.N = TRUE,
           return.t = TRUE,
           target.N = NA) {
    if (is.null(A)) {
      stop("No Matrix was found")
    }
    if (is.null(vector)) {
      stop("No vector was provided")
    }
    if (!is.matrix(A)) {
      stop("Please provide a matrix")
    }
    if (is.null(metrics)) {
      stop("Please specify metrics")
    }
    if (any(!is.matrix(A), dim(A)[1] != dim(A)[2])) {
      stop("A must be a square matrix")
    }
    if (any(is.na(A))) {
      stop("Matrix should not contain any missing values")
    }
    if (!popdemo::isIrreducible(A)) {
      stop("Matrix is reducible")
    }
    if (!popdemo::isPrimitive(A)) {
      stop("Warning: Matrix is imprimitive")
    }
    if (is.null(popname)) {
      message("no population name given, perhaps you want to specify one?")
      popname <- "pop"
    }

    msg <- character(0)

    dat <- data.frame(
      popname = popname,
      convt = NA,
      convt.N = NA,
      tt = NA,
      target.N = target.N,
      dr = NA,
      maxamp = NA,
      maxamp.t = NA,
      maxatt = NA,
      maxatt.t = NA,
      reac = NA,
      reac.t = NA
    )

    if ("all" %in% metrics) {
      metrics <- c("reac", "maxatt", "maxamp", "dr", "convt", "tt")
    }

    # reac  -------------------------------------------------------------
    if ("reac" %in% metrics) {
      reac_res <- popdemo::reac(A = A,
                                vector = vector,
                                return.N = return.N)
      if(return.N ==TRUE)
      {
        dat$reac <- reac_res$N
      }
      else(dat$reac <- reac_res)

      if(return.t ==TRUE)
      {
        dat$reac.t <- 1
      }
      else(dat$reac.t <- 999)

    }

    # maxamp ------------------------------------------------------------------
    if ("maxamp" %in% metrics) {
      maxamp_res <- calc_maxamp_or_maxatt(
        metrics = "maxamp",
        vector = vector,
        A = A,
        return.N = return.N,
        return.t = return.t
      )
      dat$maxamp     <- maxamp_res$value
      dat$maxamp.t <- maxamp_res$timestep
      if (return.t == FALSE)
      {
        dat$maxamp.t <- 999
      }
      msg <- paste(msg, maxamp_res$msg)
    }

    # maxatt ------------------------------------------------------------------
    if ("maxatt" %in% metrics) {
      maxatt_res <- calc_maxamp_or_maxatt(
        metrics = "maxatt",
        vector = vector,
        A = A,
        return.N = return.N,
        return.t = return.t
      )
      dat$maxatt     <- maxatt_res$value
      dat$maxatt.t <- maxatt_res$timestep
      if (return.t == FALSE)
      {
        dat$maxatt.t <- 999
      }
      msg <- paste(msg, maxatt_res$msg)

    }

    #tt -----------------------------------------------------------------------
    if ("tt" %in% metrics) {
      if(!is.na(target.N)){
        t_res <- list(value = 999,
                      msg = character(0))
        msg.tt <- character(0)
        tt.warning.tt <-
          tryCatch(
            tt_warn<- time_to_target(A, vector, target.N, max_time, chunk),
            warning = function(w)
              w
          )
        if (methods::is(tt.warning.tt, "warning")) {
          msg.tt <- cbind(msg.tt, (
            tt.warning.tt[1]$message
          ))

          t_res <- 999
        }
        else {
          t_res <- time_to_target(A, vector, target.N, max_time, chunk)
          msg <- paste(msg, msg.tt)  # shift the message here because later on it is not produced
        }
      }
      else{
        message("You specified tt in the metrics but did not specify
                a population abundance target in target.N, perhaps you want to specify one?")
        t_res <- 999

      }

      dat$tt <- t_res


    }

    # DAMPING RATIO -----------------------------------------------------------
    if ("dr" %in% metrics) {
      dat$dr <- popdemo::dr(A)
    }

    # convergence time --------------------------------------------------------
    if ("convt" %in% metrics) {
      convt_res <- calc_convt(
        metrics = "convt",
        vector = vector,
        A = A,
        accuracy = accuracy,
        iterations = iterations,
        return.N = return.N
      )

      dat$convt     <- convt_res$value
      dat$convt.N <- convt_res$N
    }

    if (any(is.na(dat))) {
      dat <- dat[, -which(is.na(dat))]
    }
    dat[, which(dat == 999)] <- NA

    if (verbose && length(msg) > 0) {
      attr(dat, "msg") <- msg

    }

    return(dat)
  }



#' Calculate maxamp or maxatt metric
#'
#' Internal functions used by [calc_resilience()].
#'
#' @inheritParams calc_resilience
#' @seealso [calc_resilience()]
#' @keywords internal


calc_maxamp_or_maxatt <- function(metrics,
                                  vector,
                                  A,
                                  return.N = return.N,
                                  return.t = return.t) {
  msg <- character(0)

  if (length(metrics) != 1 ||
      (!"maxamp" %in% metrics && !"maxatt" %in% metrics)) {
    stop("this function can only use 'maxamp' or 'maxatt' as metrics")
  }


  list_res <- list(value = 999,
                   timestep = 999,
                   msg = character(0))

  fn <- switch(metrics,
               maxamp = popdemo::maxamp,
               maxatt = popdemo::maxatt)

  if (vector[1] != "n") {
    tt.error.maxa <-
      tryCatch(
        maxa <- fn(
          A,
          vector = vector,
          return.N = return.N,
          return.t = return.t
        ),
        error = function(e)
          e
      )
    if (methods::is(tt.error.maxa, "error")) {
      msg <- cbind(msg, (
        paste0(
          tt.error.maxa[1]$message,
          " with the stated initial vector, Na is displayed "
        )
      ))
      list_res$value <- 999
    }
    else {
      temp <- fn(A,
                 vector = vector,
                 return.N = return.N,
                 return.t = return.t)
      if (return.N == TRUE)
      {
        list_res$value <- temp$N
      }
      else(list_res$value <- temp[[1]])

      if (return.t == TRUE) {
        list_res$timestep <- temp$t
      }

    }
  }

  list_res$msg <- msg
  list_res
}


#' Calculate convergence time
#'
#' Internal functions used by [calc_resilience()].
#'
#' @inheritParams calc_resilience
#' @seealso [calc_resilience()]
#' @keywords internal

calc_convt <- function(metrics,
                       vector,
                       A,
                       accuracy,
                       iterations,
                       return.N) {
  if (!"convt" %in% metrics) {
    stop("this function can only use 'convt'")
  }

  list_res <- list(value = 999, N = 999)


  list_res$value <- popdemo::convt(A,
                                   vector = vector,
                                   accuracy = accuracy,
                                   iterations = iterations)


  if (return.N == TRUE) {
    maxconvt <- max(list_res$value)
    projpop <- popdemo::project(
      A,
      standard.A = FALSE,
      vector = vector,
      time = (maxconvt + 1)
    )

  }
  if (return.N == FALSE) {
    maxconvt <- max(list_res$value)
    projpop <- popdemo::project(
      A,
      standard.A = TRUE,
      vector = vector / sum(vector),
      time = (maxconvt + 1)
    )
  }

  list_res$N <- projpop[((list_res$value)+1)] #small trick to take out the first value of the projection

  return(list_res)
}


#' Calculate Time to Target
#'
#' Internal functions used by [calc_resilience()].
#'
#' @inheritParams calc_resilience
#' @seealso [calc_resilience()]
#' @keywords internal
time_to_target <- function(A, n0, target, max_time = 10000, chunk = 100) {
  t <- 0
  n <- n0
  max_time = 10000
  chunk = 100
  repeat {
    # project in chunks
    proj <- popdemo::project(A, n, chunk, return.vec = TRUE)

    # check if target is reached in this chunk
    if (any(proj >= target)) {
      t_hit <- which(proj >= target)[1]
      return(t + t_hit)
    }

    # update for next chunk
    n <- proj@vec[length(proj),]
    t <- t + chunk

    # stop if exceeded max_time
    if (t >= max_time) {
      warning("The maximum projection time to identify the population target has been reached, NA will be returned")
      return(999)
    }
  }
}
