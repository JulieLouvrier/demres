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
#'                 "inertia": Calculates population inertia for a
#'                 matrix projection model.\cr
#'                 "maxamp": Calculates maximal amplification for a
#'                 matrix projection model.\cr
#'                 "maxatt": Calculates maximal attenuation for a
#'                 matrix projection model.\cr
#'                 "reac": Calculates reactivity: first time step amplification
#'                 and first time step attenuation for a matrix
#'                 projection model.\cr
#'                 "all": all of the above metrics are provided.
#' @param bounds (optional) Boolean. Set to FALSE as default. If TRUE, specifies whether the upper and  lower
#' bound should be calculated. If initial vector is not specified, the function
#' provides metrics at their upper and lower bounds, calculated based on the stage-biased vector.
#' If vector is specified, the function provides also the metrics calculated
#' based on the initial vector.
#' @param vector a numeric vector or one-column matrix describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific'
#' resilience metric
#' @param popname a character string describing the name of the population.
#' @param accuracy option for calculating convergence time: the accuracy with which to determine convergence to asymptotic growth,
#' expressed as a proportion. Set to 0.01 by default.
#' @param iterations option for calculating convergence time: the maximum number of iterations of the model. Set to 1e+05 by default. For slowly-converging models
#' and/or high specified convergence accuracy, this may need to be increased.
#' @param verbose Boolean. Set to TRUE as default. Indicates whether the messages about failure
#' to compute particular metric should be displayed or not (default = TRUE)
#' @export
#' @examples
#' data(adeliepenguin)
#'
#' set.seed(1234)
#' Cranevec1 <- runif(5)
#' Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vec to sum to 1
#' crane1 <- adeliepenguin[[1]]
#'
#' all_crane_demres <- calc_resilience(crane1, metrics = c("all"),
#' vector = Cranevec1, bounds = TRUE, popname = "blue crane", verbose = TRUE)
#'
#' @return A vector containing all the resilience metrics
#' @name calc_resilience
#' @keywords internal

calc_resilience <-
  function(A,
           metrics = "all",
           bounds = FALSE,
           vector = "n",
           popname = NULL,
           verbose = TRUE,
           accuracy = 0.01,
           iterations = 1e+05) {

    if (is.null(A)) {
      stop("No Matrix was found")
    }
    if (!is.matrix(A)){
      stop("Please provide a matrix")
    }
    if (is.null(metrics)) {
      stop("Please specify metrics")
    }
    if (any(! is.matrix(A), dim(A)[1] != dim(A)[2])) {
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

    #else{ # I commented that (Alex) since it seems useless, but perhaps you were enforcing evaluation to bypass lazy evaluation?
    #popname = popname
    #}

    msg <- character(0)

    dat <- data.frame(popname = popname,
                      convt = NA,
                      convt_lwr = NA,
                      convt_upr = NA,
                      dr = NA,
                      inertia = NA,
                      inertia_lwr = NA,
                      inertia_upr = NA,
                      maxamp = NA,
                      maxamp_upr = NA,
                      maxatt = NA,
                      maxatt_lwr = NA,
                      reac = NA,
                      reac_lwr = NA,
                      reac_upr = NA)

    if (metrics == "all") {
      metrics <- c("reac", "inertia", "maxatt", "maxamp", "dr", "convt")
    }

    # reac  -------------------------------------------------------------
    if ("reac" %in% metrics) {
      reac_res <- calc_reac_or_inertia(metrics = "reac", vector = vector, A = A, bounds = bounds)
      dat$reac     <- reac_res$value
      dat$reac_lwr <- reac_res$lwr
      dat$reac_upr <- reac_res$upr
    }

    # inertia  ----------------------------------------------------------------
    if ("inertia" %in% metrics) {
      inertia_res <- calc_reac_or_inertia(metrics = "inertia", vector = vector, A = A, bounds = bounds)
      dat$inertia     <- inertia_res$value
      dat$inertia_lwr <- inertia_res$lwr
      dat$inertia_upr <- inertia_res$upr
    }

    # maxamp ------------------------------------------------------------------
     if ("maxamp" %in% metrics) {
       maxamp_res <- calc_maxamp_or_maxatt(metrics = "maxamp", vector = vector, A = A, bounds = bounds)
       dat$maxamp     <- maxamp_res$value
       #dat$maxatt_lwr <- maxamp_res$lwr
       dat$maxamp_upr <- maxamp_res$upr
       msg <- cbind(msg, maxamp_res$msg)
    }

    # maxatt ------------------------------------------------------------------
    if ("maxatt" %in% metrics) {
      maxatt_res <- calc_maxamp_or_maxatt(metrics = "maxatt", vector = vector, A = A, bounds = bounds)
      dat$maxatt     <- maxatt_res$value
      dat$maxatt_lwr <- maxatt_res$lwr
      #dat$maxamp_upr <- maxatt_res$upr
      msg <- cbind(msg, maxatt_res$msg)

    }

    # DAMPING RATIO -----------------------------------------------------------
    if ("dr" %in% metrics) {
      dat$dr <- popdemo::dr(A)
    }

    # convergence time
    if ("convt" %in% metrics) {
      convt_res <- calc_convt(metrics = "convt",
                              vector = vector,
                              A = A,
                              bounds = bounds ,
                              accuracy = accuracy,
                              iterations = iterations)

      dat$convt     <- convt_res$value
      dat$convt_lwr <- convt_res$lwr
      dat$convt_upr <- convt_res$upr
    }

    if (any(is.na(dat))){
    dat <- dat[, -which(is.na(dat))] #taking out columns with NAs in them
     }
    dat[, which(dat == 999)] <- NA

    if (verbose && length(msg) > 0) {
      #message(msg)
      attr(dat, "msg") <- msg

    }

    return(dat)
  }


#' Calculate reactivity or inertia metric
#'
#' Internal functions used by [calc_resilience()].
#'
#' @inheritParams calc_resilience
#' @seealso [calc_resilience()]
#'
calc_reac_or_inertia <- function(metrics, vector, A, bounds) {

  if (length(metrics) != 1 || (!"reac" %in% metrics && !"inertia" %in% metrics)) {
    stop("this function can only use 'reac' or 'inertia' as metrics")
  }

  list_res <- list(value = 999, lwr = 999, upr = 999)

  fn <- switch(metrics,
               reac = popdemo::reac,
               inertia = popdemo::inertia)

  if (vector[1] != "n") {
    list_res$value <- fn(A, vector = vector)
  } else {
    if (!bounds) {
      stop(paste("Please specify bound=\"upper\", bound=\"lower\" or specify vector for", metrics))
    }
  }

  if (bounds) {
    list_res$lwr <- fn(A, bound = "lower")
    list_res$upr <- fn(A, bound = "upper")
  }

  list_res
}

#' Calculate maxam or maxatt metric
#'
#' Internal functions used by [calc_resilience()].
#'
#' @inheritParams calc_resilience
#' @seealso [calc_resilience()]
#'
calc_maxamp_or_maxatt <- function(metrics, vector, A, bounds) {
  msg <- character(0)

  if (length(metrics) != 1 || (!"maxamp" %in% metrics && !"maxatt" %in% metrics)) {
    stop("this function can only use 'maxamp' or 'maxatt' as metrics")
  }


  list_res <- list(value = 999, lwr = 999, upr = 999, msg = character(0))

  fn <- switch(metrics,
               maxamp = popdemo::maxamp,
               maxatt = popdemo::maxatt)

  if (vector[1] != "n") {
    tt.error.maxa <-
      tryCatch(
        maxa <- fn(A, vector = vector),
        error = function(e) e
      )
    if (methods::is(tt.error.maxa, "error")) {
      msg <- cbind(msg, (paste0(tt.error.maxa[1]$message, " with the stated initial vector, Na is displayed ")))
      list_res$value <- 999
    }
    else {
      list_res$value <- fn(A, vector = vector)
    }
  } else {
    if (!bounds) {
      stop(paste("Please specify bound=\"upper\", bound=\"lower\" or specify vector for", metrics))
    }
  }
  if (bounds) {
    list_res$lwr <- fn(A)
    list_res$upr <- fn(A)
    if(metrics == "maxamp") {
      message_maxamp <- c("The lower bound of maximum amplification cannot be computed. Therefore, the lower maximum attenuation is calculated using the default stage biased vector")
      msg <- cbind(msg, message_maxamp)
    }

    if(metrics == "maxatt") {
      message_maxatt <- c("The upper bound of maximum attenuation cannot be computed. Therefore, the upper maximum amplification is calculated using the default stage biased vector")
      msg <- cbind(msg, message_maxatt)
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
#'
calc_convt <- function(metrics, vector, A, bounds, accuracy, iterations) {

  if (!"convt" %in% metrics) {
    stop("this function can only use 'convt'")
  }

  list_res <- list(value = 999, lwr = 999, upr = 999)

  if (vector[1] != "n") {
    list_res$value <- popdemo::convt(A, vector = vector, accuracy = accuracy, iterations = iterations)
  } else {
    if (!bounds) {
      stop(paste("Please specify bound=\"upper\", bound=\"lower\" or specify vec for", metrics))
    }
  }

  if (bounds) {
    list_res$lwr <- min(popdemo::convt(A, accuracy = accuracy, iterations = iterations))
    list_res$upr <- max(popdemo::convt(A, accuracy = accuracy, iterations = iterations))
  }

  list_res
}
