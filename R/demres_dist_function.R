#' Calculates distance between the time-varying resilience metric and the time-
#' constant one
#'
#' `demres_dist` Calculates distance between the time-varying resilience
#' metric and the time-constant one
#'
#' @param table A dataframe containing all the resilience metrics calculated
#' with the demres function
#' @param metric "reac": Reactivity: first-timestep amplification
#'                 and first-timestep attenuation for a population matrix
#'                 projection model.
#'                 "inertia": Population inertia for a population
#'                 matrix projection model.
#'                 "dr": Damping ratio of a given population
#'                 matrix projection model.
#'                 "maxamp": Maximal amplification for a population
#'                 matrix projection model.
#'                 "maxatt": Maximal attenuation for a population
#'                 matrix projection model.
#' @param measure "RMSE": calculates the RMSE (sqrt(mean((TV-TC)^2))
#' with TV: the time-Varying resilience metric and TC the time constant one)
#'                 "rRMSE":calculates the relative RMSE
#' (sqrt(mean((TV-TC)^2)) / sd(TV) with TV: the time-Varying resilience metric
#' and TC the time constant )
#'                 "MAPE": calculates the MAPE (mean(abs(TV - TC)) with TV:
#'                 the time-Varying resilience metric and TC the time constant)
#'                 "all": calculates all of the above measures
#' @examples
#' # load data
#' comadre <- cdb_fetch("comadre")
#'
#' #selecting the blue crane
#' blue_crane <- comadre[comadre@data$SpeciesAccepted  == "Anthropoides paradiseus", ]
#'
#' #extracting matrices
#' blue_crane_matA <- matA(blue_crane)
#'
#' # simulate an initial vector
#' Cranevec1 <- runif(5)
#' Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vec to sum to 1
#'
#'
#' BC_TVTC_demres <-
#'   demres(
#'     blue_crane_matA,
#'     metric = "all",
#'     bounds = TRUE,
#'     initvec = Cranevec1,
#'     popname = "blue crane",
#'     time = "both"
#'   )
#'
#'
#' dist_BC <- demres_dist(table = BC_TVTC_demres, metric = "inertia",
#' measure = "all")
#'
#' @return A dataframe displaying the mdistance measures for the selected metric
#' @export
#' @name demres_dist

demres_dist <- function(table,
                        metric,
                        measure) {
  if (length(grep(metric, colnames(table))) == 0) {
    stop("The requested metric is not available in the provided dataframe, try another metric")
  }

  if(length(grep("_TV", names(table))) == 0 | length(grep("_TC", names(table))) == 0){
    stop("To calculte the distance metrics, both time-varying and time-constant approaches are necessary")

  }

  else {
    #time varying
    table_metric_upr_TV <-
      table[, grep(paste0(metric, "_upr_TV"), colnames(table))]
    table_metric_lwr_TV <-
      table[, grep(paste0(metric, "_lwr_TV"), colnames(table))]
    table_metric_initvect_TV <-
      table[, grep(paste0(metric, "_TV"), colnames(table))]

    #time constant
    table_metric_upr_TC <-
      table[, grep(paste0(metric, "_upr_TC"), colnames(table))]
    table_metric_lwr_TC <-
      table[, grep(paste0(metric, "_lwr_TC"), colnames(table))]
    table_metric_initvect_TC <-
      table[, grep(paste0(metric, "_TC"), colnames(table))]



    if (measure == "RMSE") {
      if(length(table_metric_lwr_TV) == 0){
        RMSE_lwr = NA
      }
      else{
        RMSE_lwr <-
          RMSE(TV = table_metric_lwr_TV, TC = table_metric_lwr_TC)
      }
      if(length(table_metric_initvect_TV) == 0){
        RMSE_init = NA
      }
      else{
        RMSE_init <-
          RMSE(TV = table_metric_initvect_TV, TC = table_metric_initvect_TC)
      }
      if(length(table_metric_upr_TV) == 0) {
        RMSE_upr = NA
      }
      else{
        RMSE_upr <-
          RMSE(TV = table_metric_upr_TV, TC = table_metric_upr_TC)
      }

      RMSE_res <- c(RMSE_lwr,
                    RMSE_init,
                    RMSE_upr)
      names(RMSE_res) <- c(paste0("RMSE_", metric, "_lwr"), paste0("RMSE_", metric, "_initvect"), paste0("RMSE_", metric, "_upr"))

      distance_demres <- RMSE_res
    }

    if (measure == "rRMSE") {
      if(length(table_metric_lwr_TV) == 0){
        rRMSE_lwr = NA
      }
      else{
        rRMSE_lwr <-
          rRMSE(TV = table_metric_lwr_TV, TC = table_metric_lwr_TC)
      }
      if(length(table_metric_initvect_TV) == 0){
        rRMSE_init = NA
      }
      else{
        rRMSE_init <-
          rRMSE(TV = table_metric_initvect_TV, TC = table_metric_initvect_TC)
      }
      if(length(table_metric_upr_TV) == 0) {
        rRMSE_upr = NA
      }
      else{
        rRMSE_upr <-
          rRMSE(TV = table_metric_upr_TV, TC = table_metric_upr_TC)
      }

      rRMSE_res <- c(rRMSE_lwr,
                     rRMSE_init,
                     rRMSE_upr)
      names(rRMSE_res) <- c(paste0("rRMSE_", metric, "_lwr"), paste0("rRMSE_", metric, "_initvect"), paste0("rRMSE_", metric, "_upr"))

      distance_demres <- rRMSE_res
    }
    if (measure == "MAPE") {
      if(length(table_metric_lwr_TV) == 0){
        MAPE_lwr = NA
      }
      else{
        MAPE_lwr <-
          MAPE(TV = table_metric_lwr_TV, TC = table_metric_lwr_TC)
      }
      if(length(table_metric_initvect_TV) == 0){
        MAPE_init = NA
      }
      else{
        MAPE_init <-
          MAPE(TV = table_metric_initvect_TV, TC = table_metric_initvect_TC)
      }
      if(length(table_metric_upr_TV) == 0) {
        MAPE_upr = NA
      }
      else{
        MAPE_upr <-
          MAPE(TV = table_metric_upr_TV, TC = table_metric_upr_TC)
      }

      MAPE_res <- c(MAPE_lwr,
                    MAPE_init,
                    MAPE_upr)
      names(MAPE_res) <- c(paste0("MAPE_", metric, "_lwr"), paste0("MAPE_", metric, "_initvect"), paste0("MAPE_", metric, "_upr"))

      distance_demres <- MAPE_res
    }
    if (measure == "all") {
      if(length(table_metric_lwr_TV) == 0){
        RMSE_lwr = NA
      }
      else{
        RMSE_lwr <-
          RMSE(TV = table_metric_lwr_TV, TC = table_metric_lwr_TC)
      }
      if(length(table_metric_initvect_TV) == 0){
        RMSE_init = NA
      }
      else{
        RMSE_init <-
          RMSE(TV = table_metric_initvect_TV, TC = table_metric_initvect_TC)
      }
      if(length(table_metric_upr_TV) == 0) {
        RMSE_upr = NA
      }
      else{
        RMSE_upr <-
          RMSE(TV = table_metric_upr_TV, TC = table_metric_upr_TC)
      }

      RMSE_res <- c(RMSE_lwr,
                    RMSE_init,
                    RMSE_upr)
      if(length(table_metric_lwr_TV) == 0){
        rRMSE_lwr = NA
      }
      else{
        rRMSE_lwr <-
          rRMSE(TV = table_metric_lwr_TV, TC = table_metric_lwr_TC)
      }
      if(length(table_metric_initvect_TV) == 0){
        rRMSE_init = NA
      }
      else{
        rRMSE_init <-
          rRMSE(TV = table_metric_initvect_TV, TC = table_metric_initvect_TC)
      }
      if(length(table_metric_upr_TV) == 0) {
        rRMSE_upr = NA
      }
      else{
        rRMSE_upr <-
          rRMSE(TV = table_metric_upr_TV, TC = table_metric_upr_TC)
      }

      rRMSE_res <- c(rRMSE_lwr,
                     rRMSE_init,
                     rRMSE_upr)

      if(length(table_metric_lwr_TV) == 0){
        MAPE_lwr = NA
      }
      else{
        MAPE_lwr <-
          MAPE(TV = table_metric_lwr_TV, TC = table_metric_lwr_TC)
      }
      if(length(table_metric_initvect_TV) == 0){
        MAPE_init = NA
      }
      else{
        MAPE_init <-
          MAPE(TV = table_metric_initvect_TV, TC = table_metric_initvect_TC)
      }
      if(length(table_metric_upr_TV) == 0) {
        MAPE_upr = NA
      }
      else{
        MAPE_upr <-
          MAPE(TV = table_metric_upr_TV, TC = table_metric_upr_TC)
      }

      MAPE_res <- c(MAPE_lwr,
                    MAPE_init,
                    MAPE_upr)

      distance_demres <- data.frame(RMSE = RMSE_res,
                                    rRMSE = rRMSE_res,
                                    MAPE = MAPE_res)

      rownames(distance_demres) <-  c(paste0(metric, "_lwr"),
                                      paste0(metric, "_initvect"),
                                      paste0(metric, "_upr"))


    }

  }

  if(is.vector(distance_demres) == TRUE) {
    if(!length(which(is.na(distance_demres))) == 0){
    distance_demres <- distance_demres[-which(is.na(distance_demres))]
    }
    if(metric == "dr") {
      names(distance_demres) <- sub(x = names(distance_demres), "_initvect.*", "")
    }

  }
  if(is.data.frame(distance_demres) == TRUE) {
    if(!length(which(is.na(distance_demres))) == 0){
    distance_demres <- distance_demres[-which(is.na(distance_demres$RMSE)),]
    }
    if(metric == "dr") {
      rownames(distance_demres) <- "dr"
    }
  }


  return(distance_demres)

}

