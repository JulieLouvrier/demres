#' Helper functions for demres_dist function from the package "demres"
#'
#' \code{helper_functions} Calculates distance between the time-varying resilience
#' metric and the time-constant one
#'
#' @param TV A vector containing values of time-varying demographic resilience
#' for one specific  metric, calculated with demres
#' @param TC A vector containing the value of the time-constant demographic
#'  resilience for the same one specific  metric as for TV, calculated with demres




RMSE <- function(TV, TC) {
  sqrt(mean((TV - TC) ^ 2, na.rm = T))
}

rRMSE <- function(TV, TC) {
  sqrt(mean((TV - TC) ^ 2, na.rm = T)) / sd(TV, na.rm = T)
}

MAPE <- function(TV, TC) {
  mean(abs(TV - TC), na.rm = T)
}
