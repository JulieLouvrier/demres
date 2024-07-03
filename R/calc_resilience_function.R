#' Provides resilience metrics from the package popdemo for one matrix
#'
#' `calc_resilience` calculates resilience metrics of a population based
#' on a matrix population model
#'
#' This function compiles different metrics of resilience, given by the popdemo
#' package based on a population matrix projection model.
#'
#'
#' @param A a square, primitive, irreducible, non-negative numeric matrix of any
#' dimension
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
#' @param bounds (optional) if TRUE, specifies whether the upper and  lower bound
#' should be calculated
#' if vec is not specified, the function provides metrics in their upper and
#' lower bound, calculated based on the stage-biased vec
#' if vec is specified, the function provides also the metrics calculated based
#' on the inital vec
#' @param initvec a numeric vec or one-column matrix describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific'
#' maximal amplification
#' @param popname a character string describing the name of the population
#' @param verbose a boolean indicating if messages about failure to compute particular metrics should be displayed or not (default = TRUE)
#' @export
#' @examples
#' data(Tort, package = "popdemo")
#'
#' Tortvec1 <- runif(8) # create initial vec
#' Tortvec1 <- Tortvec1/sum(Tortvec1) #scales the vec to sum to 1
#'
#' all_tort_demres <- calc_resilience(Tort, metrics = c("all"),
#' initvec = Tortvec1, bounds = TRUE, popname = "Tortoise")
#'
#' @return A vector containing all the resilience metrics
#' @name calc_resilience

calc_resilience <-
  function(A,
           metrics,
           bounds = FALSE,
           initvec = "n",
           popname = NULL,
           verbose = TRUE) {

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
      print("Warning: Matrix is imprimitive")
    }

    if(is.null(popname)) {
      message("no population name given, perhaps you want to specify one?")
      popname <- "pop1"
    }

    else{
    popname = popname
    }

    msg <- character(0)

    dat <- data.frame(popname = popname,
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
      metrics <- c("reac", "inertia", "maxatt", "maxamp", "dr")
    }

    # reac  -------------------------------------------------------------
    if ("reac" %in% metrics) {
      reac_res <- calc_reac_or_inertia(metrics = "reac", initvec = initvec, A = A, bounds = bounds)
      dat$reac     <- reac_res$value
      dat$reac_lwr <- reac_res$lwr
      dat$reac_upr <- reac_res$upr
    }

    # inertia  ----------------------------------------------------------------
    if ("inertia" %in% metrics) {
      inertia_res <- calc_reac_or_inertia(metrics = "inertia", initvec = initvec, A = A, bounds = bounds)
      dat$inertia     <- inertia_res$value
      dat$inertia_lwr <- inertia_res$lwr
      dat$inertia_upr <- inertia_res$upr
    }

    # maxamp ------------------------------------------------------------------
    if ("maxamp" %in% metrics) {
      if (initvec[1] == "n") {
        if (!bounds) {
          stop("Please specify bound=\"upper\", bound=\"lower\" or specify vec for maxamp")
        } else {
          message(
            "The lower bound of maximum amplification cannot be computed \n Therefore, the lower maximum attenuation is calculated using the default stage biased vector"
          )
          dat$maxatt_lwr <- popdemo::maxatt(A)
          dat$maxamp_upr <- popdemo::maxamp(A)
        }
      }

      else{
        tt.error.maxamp <-
          tryCatch(
            maxamp <- popdemo::maxamp(A, vec = initvec),
            error = function(e)
              e
          )
        if (methods::is(tt.error.maxamp, "error")) {
          message(paste0(tt.error.maxamp[1]$message, " with the stated initial vector, Na is displayed"))

          dat$maxamp <- 999
        }
        else{
          dat$maxamp <- maxamp
        }


        if (bounds) {
          print(
            "Warning: The lower bound of maximum amplification cannot be computed \n Therefore, the lower maximum attenuation is calculated using the default stage biased vector"
          )
          dat$maxatt_lwr <- popdemo::maxatt(A)
          dat$maxamp_upr <- popdemo::maxamp(A)
        }
      }
    }

    # maxatt ------------------------------------------------------------------
    if ("maxatt" %in% metrics) {
      if (initvec[1] == "n") {
        if (!bounds) {
          stop("Please specify bound=\"upper\", bound=\"lower\" or specify vec for maxatt")
        }

        if (bounds) {
          print(
            "Warning: The upper bound was requested with maximum attenuation \n Therefore, the upper maximum amplification is calculated using the default stage biased vector"
          )
          dat$maxatt_lwr <- popdemo::maxatt(A)
          dat$maxamp_upr <- popdemo::maxamp(A)
        }
      }

      else{
        tt.error.maxatt <-
          tryCatch(
            maxatt <- popdemo::maxatt(A, vec = initvec),
            error = function(e)
              e
          )
        if (methods::is(tt.error.maxatt, "error")) {
          msg <- c(msg, paste0(tt.error.maxatt[1]$message, ", with the stated initial vector, Na is displayed"))
          dat$maxatt <- 999
        }
        else{
          dat$maxatt <- maxatt
        }

        if (bounds) {
          message(
            "The upper bound was requested with maximum attenuation \n Therefore, the upper maximum amplification is calculated using the default stage biased vector"
          )
          dat$maxatt_lwr <- popdemo::maxatt(A)
          dat$maxamp_upr <- popdemo::maxamp(A)
        }
      }
    }

    # DAMPING RATIO -----------------------------------------------------------
    if ("dr" %in% metrics) {
      dat$dr <- popdemo::dr(A)
    }

    # ALL --------------------------------------------------------------------
    if ("all" %in% metrics) {
      if (initvec[1] == "n") {
        if (!bounds) {
          stop(
            "Please specify bound=\"upper\", bound=\"lower\" or specify vec for reac and inertia"
          )
        }
        dat$dr <- popdemo::dr(A)

        if (bounds) {
          dat$reac_lwr <- popdemo::reac(A, bound = "lower")
          dat$inertia_lwr <- popdemo::inertia(A, bound = "lower")
          dat$maxatt_lwr <- popdemo::maxatt(A)
          dat$reac_upr <- popdemo::reac(A, bound = "upper")
          dat$inertia_upr <- popdemo::inertia(A, bound = "upper")
          dat$maxamp_upr <- popdemo::maxamp(A)

        }
      } else {
        dat$dr <- popdemo::dr(A)
        dat$reac <- popdemo::reac(A, vec = initvec)
        dat$inertia <- popdemo::inertia(A, vec = initvec)

        tt.error.maxamp <-
          tryCatch(
            maxamp <- popdemo::maxamp(A, vec = initvec),
            error = function(e)
              e
          )
        if (methods::is(tt.error.maxamp, "error")) {
          message(paste0(tt.error.maxamp[1]$message, ", with the stated initial vector, Na is displayed"))
          dat$maxamp <- 999
        }
        else{
          dat$maxamp <- popdemo::maxamp(A, vec = initvec)
        }
        tt.error.maxatt <-
          tryCatch(
            maxatt <- popdemo::maxatt(A, vec = initvec),
            error = function(e)
              e
          )
        if (methods::is(tt.error.maxatt, "error")) {
          msg <- c(msg, paste0(tt.error.maxatt[1]$message, ", with the stated initial vector, Na is displayed"))
          dat$maxatt <- 999
        }
        else{
          dat$maxatt <- popdemo::maxatt(A, vec = initvec)
        }
        if (bounds) {
          dat$reac_lwr <- popdemo::reac(A, bound = "lower")
          dat$inertia_lwr <- popdemo::inertia(A, bound = "lower")
          dat$maxatt_lwr <- popdemo::maxatt(A)
          dat$reac_upr <- popdemo::reac(A, bound = "upper")
          dat$inertia_upr <- popdemo::inertia(A, bound = "upper")
          dat$maxamp_upr <- popdemo::maxamp(A)

        }
      }
    }
    if ("TRUE" %in% is.na(dat)){
    dat <- dat[,-which(is.na(dat))] #taking out columns with NAs in them
    }
    dat[,which(dat == 999)] <- NA

    print(verbose)
    print(msg)
    if (verbose && length(msg) > 0) {
      message(msg)
    }
    attr(dat, "msg") <- msg

    return(dat)
  }


#' Calculate reactivity or inertia metric
#'
#' Internal functions used by [calc_resilience()].
#'
#' @inheritParams calc_resilience
#' @seealso [calc_resilience()]
#'
calc_reac_or_inertia <- function(metrics, initvec, A, bounds) {

  if (length(metrics) != 1 || (!"reac" %in% metrics && !"inertia" %in% metrics)) {
    stop("this function can only use 'reac' or 'inertia' as metrics")
  }

  list_res <- list(value = NA, lwr = NA, upr = NA)

  fn <- switch(metrics,
               reac = popdemo::reac,
               inertia = popdemo::inertia)

  if (initvec[1] != "n") {
    list_res$value <- fn(A, vec = initvec)
  } else {
    if (!bounds) {
      stop(paste("Please specify bound=\"upper\", bound=\"lower\" or specify vec for", metrics))
    }
  }

  if (bounds) {
    list_res$lwr <- fn(A, bound = "lower")
    list_res$upr <- fn(A, bound = "upper")
  }

  list_res
}
