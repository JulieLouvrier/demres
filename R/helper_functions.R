#' Helper functions for demres_dist function
#'
#' @param TV A vector containing values of time-varying demographic resilience
#' for one specific metric
#' @param TC A vector containing the value of the time-constant demographic
#'  resilience for the same one specific metric as for TV
#' @return Values of the distance measure
#' @name helper_functions
#' @keywords internal
NULL

#' @describeIn helper_functions
#' `RMSE` Calculates RMSE between the time-varying resilience
#' metric and the time-constant one
#' @export
RMSE <- function(TV, TC) {
  sqrt(mean((TV - TC) ^ 2, na.rm = TRUE))
}

#' @describeIn helper_functions
#' `rRMSE` Calculates rRMSE between the time-varying resilience
#' @export
rRMSE <- function(TV, TC) {
  sqrt(mean((TV - TC) ^ 2, na.rm = TRUE)) / stats::sd(TV, na.rm = TRUE)
}

#' @describeIn helper_functions
#' `MAPE` Calculates MAPE between the time-varying resilience
#' metric and the time-constant one
MAPE <- function(TV, TC) {
  mean(abs(TV - TC)/TC, na.rm = TRUE)
}

