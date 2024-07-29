#' Calculates distance between the time-varying resilience metric and the time-
#' constant one
#'
#' `summary` Calculates distance between the time-varying resilience
#' metric and the time-constant one:
#' "RMSE": sqrt(mean((TV-TC)^2))
#' with TV: the time-Varying resilience metric and TC the time constant one
#' "rRMSE": sqrt(mean((TV-TC)^2)) / sd(TV)
#' with TV: the time-Varying resilience metric and TC the time constant
#' "MAPE": mean(abs(TV - TC))
#' with TV: the time-Varying resilience metric and TC the time constant
#' @param table A dataframe containing all the resilience metrics calculated
#' with the resilience function
#' @name demres_dist
#'
#' @examples
#' # load data
#' data(bluecrane)
#'
#' # simulate an initial vector
#' set.seed(1234)
#' Cranevec1 <- runif(5)
#' Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vec to sum to 1
#'
#'
#' BC_TVTC_demres <-
#'   resilience(
#'     listA = bluecrane,
#'     metrics = "all",
#'     bounds = TRUE,
#'     vector = Cranevec1,
#'     TDvector = FALSE,
#'     popname = "blue crane",
#'     time = "both",
#'     verbose = TRUE
#'   )
#'
#' dist_BC <- summary(BC_TVTC_demres)
#'
#' @return A dataframe displaying the distance measures for the metrics that are present in the table
#' @export

demres_dist <- function(table) {

    if(length(grep("_TV", names(table))) == 0 || length(grep("_TC", names(table))) == 0){
    stop("To calculte the distance metrics, both time-varying and time-constant approaches are necessary")

  }

  else {

    unique_combis <- unlist(strsplit(grep('TV', grep('[TVTC]', colnames(table), value = TRUE), value = TRUE), "_TV"))

    RMSE_dist <- unlist(lapply(unique_combis, FUN = function(x){RMSE(TV = unlist(table[grep(paste0(x, '_TV'), colnames(table), value= TRUE)]),
                                                                TC = unlist(table[grep(paste0(x, '_TC'), colnames(table), value= TRUE)]))
    }))

    rRMSE_dist <- unlist(lapply(unique_combis, FUN = function(x){rRMSE(TV = unlist(table[grep(paste0(x, '_TV'), colnames(table), value= TRUE)]),
                                                                  TC = unlist(table[grep(paste0(x, '_TC'), colnames(table), value= TRUE)]))
    }))

    MAPE_dist <- unlist(lapply(unique_combis, FUN = function(x){MAPE(TV = unlist(table[grep(paste0(x, '_TV'), colnames(table), value= TRUE)]),
                                                                TC = unlist(table[grep(paste0(x, '_TC'), colnames(table), value= TRUE)]))
    }))

    distance_demres <- t(data.frame(RMSE = RMSE_dist,
                                  rRMSE = rRMSE_dist,
                                  MAPE = MAPE_dist))

    colnames(distance_demres) <- unique_combis

  return(distance_demres)

  }
}

