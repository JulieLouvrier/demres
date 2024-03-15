RMSE <- function(TV, TC) {
  sqrt(mean((TV - TC) ^ 2, na.rm = T))
}

rRMSE <- function(TV, TC) {
  sqrt(mean((TV - TC) ^ 2, na.rm = T)) / sd(TV, na.rm = T)
}

MAPE <- function(TV, TC) {
  mean(abs(TV - TC), na.rm = T)
}