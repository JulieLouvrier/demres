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
#' @name demres_summary
#'
#' @examples
#' # load data
#' data(adeliepenguin)
#'
#' # simulate an initial vector
#' set.seed(125435)
#' vector_list <- lapply(1:28, function(i) runif(2))
#' AP_demres <-
#'   resilience(
#'     listA = adeliepenguin,
#'     metrics = "all",
#'     vector = vector_list,
#'     TDvector = FALSE,
#'     popname = "adelie penguin",
#'     verbose = TRUE,
#'     return.N = TRUE,
#'     return.t = TRUE
#'   )
#'
#'
#' summary_AP <- summary(AP_demres, fn = list(mean, sd, var))
#'
#' It is also possible to compile your own functions, for example
#'
#' coeffvar <- function(data, na.rm = TRUE){ # have to soecify na.rm = TRUE here
#' CV <- sd(data, na.rm = TRUE) / mean(data, na.rm = TRUE) * 100
#' }
#'
#' AP_CV <- summary(AP_TVTC_demres, fn = list(coeffvar))
#'
#' Or a combination
#' AP_mix <- summary(AP_TVTC_demres, fn = list(coeffvar, mean, sd))
#'
#'
#' @return A data frame displaying the distance measures for the metrics that are present in the table
#' @export

demres_summary <- function(table, fn = list(mean, sd)) {
  sel_cols <- colnames(table)[colnames(table) %in% c("convt", "dr", "maxamp", "maxatt", "reac")]
  distance_demres <- lapply(table[sel_cols],
                            summary.temp, fn = fn)

  res_table <- do.call(rbind.data.frame, distance_demres)
  colnames(res_table) <- as.character(substitute(fn))[-1]
  res_table

}


#' Calculate maxam or maxatt metric
#'
#' Internal functions used by [demres_summary()].
#'
#' @inheritParams demres_summary
#' @seealso [demres_summary()]
#'
summary.temp <- function(x, fn = fn) {
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
    f(x, na.rm = TRUE)
  }, simplify = FALSE)

  ## extract names from unnamed list:
  if (length(fn) > 0) {
    names(res) <- as.character(substitute(fn))[-1]
  }

  # extract names from named list (take priority):
  if (length(names(fn) > 0)) {
    names(res) <- names(fn)
  }
    res
}
