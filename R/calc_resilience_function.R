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
#' if vector is not specified, the function provides metrics in their upper and
#' lower bound, calculated based on the stage-biased vector
#' if vector is specified, the function provides also the metrics calculated based
#' on the inital vector
#' @param vector a numeric vector or one-column matrix describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific'
#' maximal amplification
#' @param popname a character string describing the name of the population
#' @param verbose a boolean indicating if messages about failure to compute particular metrics should be displayed or not (default = TRUE)
#' @export
#' @examples
#' data(Tort, package = "popdemo")
#'
#' Tortvec1 <- runif(8) # create initial vector
#' Tortvec1 <- Tortvec1/sum(Tortvec1) #scales the vector to sum to 1
#'
#' all_tort_demres <- calc_resilience(Tort, metrics = c("all"),
#' vector = Tortvec1, bounds = TRUE, popname = "Tortoise", verbose = FALSE)
#'
#' @return A vector containing all the resilience metrics
#' @name calc_resilience
#' @keywords internal

calc_resilience <-
  function(A,
           metrics,
           bounds = FALSE,
           vector = "n",
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
      stop("Warning: Matrix is imprimitive")
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
       dat$maxatt_lwr <- maxamp_res$lwr
       dat$maxamp_upr <- maxamp_res$upr
    }

    # maxatt ------------------------------------------------------------------
    if ("maxatt" %in% metrics) {
      maxatt_res <- calc_maxamp_or_maxatt(metrics = "maxatt", vector = vector, A = A, bounds = bounds)
      dat$maxatt     <- maxatt_res$value
      dat$maxatt_lwr <- maxatt_res$lwr
      dat$maxamp_upr <- maxatt_res$upr
    }

    # DAMPING RATIO -----------------------------------------------------------
    if ("dr" %in% metrics) {
      dat$dr <- popdemo::dr(A)
    }

    if ("TRUE" %in% is.na(dat)){
    dat <- dat[,-which(is.na(dat))] #taking out columns with NAs in them
    }
    dat[,which(dat == 999)] <- NA

    # print(verbose)
    # print(msg)
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
      stop(paste("Please specify bound=\"upper\", bound=\"lower\" or specify vec for", metrics))
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

  if (length(metrics) != 1 || (!"maxamp" %in% metrics && !"maxatt" %in% metrics)) {
    stop("this function can only use 'maxamp' or 'maxatt' as metrics")
  }


  list_res <- list(value = 999, lwr = 999, upr = 999)

  fn <- switch(metrics,
               maxamp = popdemo::maxamp,
               maxatt = popdemo::maxatt)

  if (vector[1] != "n") {
    tt.error.maxa <-
      tryCatch(
        maxa <- fn(A, vector = vector),
        error = function(e)
          e
      )
    if (methods::is(tt.error.maxa, "error")) {
      message(paste0(tt.error.maxa[1]$message, " with the stated initial vector, Na is displayed"))
      list_res$value <- 999
    }
    else{
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
      message("The lower bound of maximum amplification cannot be computed
            Therefore, the lower maximum attenuation is calculated using the default stage biased vector")
    }

    if(metrics == "maxatt") {
      message("The upper bound of maximum attenuation cannot be computed
            Therefore, the upper maximum amplification is calculated using the default stage biased vector")
    }
  }

  list_res
}
