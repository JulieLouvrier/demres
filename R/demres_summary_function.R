#' Calculates the distance between resilience metrics calculated with time-varying
#' and time-constant approaches
#'
#' The function `summary` calls `demres_summary` to calculate distance between resilience metrics calculated with
#' time-varying and time-constant approaches:
#' "RMSE": sqrt(mean((TV-TC)^2))
#' with TV: the time-varying resilience metric and TC the time constant one
#' "rRMSE": sqrt(mean((TV-TC)^2)) / sd(TV)
#' with TV: the time-varying resilience metric and TC the time constant
#' "MAPE": mean(abs(TV - TC))/TC
#' with TV: the resilience metric calculated using the time-varying approach
#' and TC - using the time-constant approach.
#' @param table A dataframe containing all the resilience metrics calculated
#' with the resilience function
#' @param f A character specifying whether the output should be shown in
#' "long" (demographic resilience metrics as row names) or in "wide" (demographic
#' resilience metrics as column names) format. Defaults to "wide".
#' @name demres_summary
#'
#' @examples
#' # load data
#' data(adeliepenguin)
#'
#' # simulate an initial vector
#' set.seed(1234)
#' penguinvec1 <- runif(5)
#' penguinvec1 <- penguinvec1/sum(penguinvec1) #scales the vector to sum up to 1
#'
#' AP_TVTC_demres <-
#'   resilience(
#'     listA = adeliepenguin,
#'     metrics = "all",
#'     vector = penguinvec1,
#'     TDvector = FALSE,
#'     popname = "adelie penguin",
#'     time = "both",
#'     verbose = TRUE
#'   )
#'
#' dist_AP <- summary(AP_TVTC_demres)
#'
#' @return A data frame displaying the distance measures for the metrics that are present in the table
#' @export

demres_summary <- function(table, f = 'wide') {
    #
    # colnames(distance_demres) <- unique_combis
  distance_demres <- lapply(table[c("convt", "convt.N", "dr", "maxamp")], #here have to make more flexible and select all the columns with numeric values
                            summary.temp)

    # if(f == 'wide'){
    #   return(distance_demres)
    # }
    # if(f == 'long'){
    #   return(t(distance_demres))
    # }
    # else {
    #   stop("The function only takes two possible values for 'f': either 'long' or 'wide'")
    # }
  # }
}


#' Calculate maxam or maxatt metric
#'
#' Internal functions used by [demres_summary()].
#'
#' @inheritParams demres_summary
#' @seealso [demres_summary()]
#'
summary.temp <- function(x, fn = list(mean, sd)) {
  ## special case when no list
  if (!is.list(fn)) {
    if(length(fn) > 1) stop("Arg `fn` should be a list of unquoted function names")
    name.fn <- as.character(substitute(fn))
    fn <- list(fn)
    names(fn) <- name.fn
  }

  ## apply the function, easy:
  res <- sapply(fn, \(f) {
    if (!is.function(f)) stop("Arg `fn` should be a list of unquoted function names")
    f(x)
  }, simplify = FALSE)

  ## extract names from unnamed list:
  if (length(fn) > 0) {
    names(res) <- as.character(substitute(fn))[-1]
  }

  ## extract names from named list (take priority):
  if (length(names(fn) > 0)) {
    names(res) <- names(fn)
  }

  res
}
