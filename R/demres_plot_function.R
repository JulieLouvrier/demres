#' Plot the resulting demographic resilience metrics
#'
#' `demres_plot` provides a plot to visually inspect the resilience metric
#' along a time axis
#'
#' @param table A dataframe containing all the resilience metrics calculated
#' with the demres function
#' @param metric: "reac": Reactivity: first-timestep amplification
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
#' @param RMSE: (optional) if TRUE: calculates the RMSE (sqrt(mean((TV-TC)^2))
#' with TV: the time-Varying resilience metric and TC the time constant one)
#' and adds it on the plot.
#' @param rRMSE: (optional) if TRUE: calculates the relative RMSE
#' (sqrt(mean((TV-TC)^2)) / sd(TV) with TV: the time-Varying resilience metric
#' and TC the time constant ) and adds it on the plot
#' @param MAPE: (optional) if TRUE: calculates the MAPE ((mean(abs(TV - TC))/100)
#' and adds it on the plot
#' @param plotname: name of the plot with directory. By default:
#' plotname = paste0(getwd(), "/plot_demres_", metric, ".pdf")
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
#'     metrics = "all",
#'     bounds = TRUE,
#'     initvec = Cranevec1,
#'     popname = "blue crane",
#'     time = "both"
#'   )
#'
#'
#' #plotting with RMSE
#'
#' metric = "dr"
#' demres_plot(table = BC_TVTC_demres,
#'             metric = metric,
#'             plotname = paste0(getwd(),
#'             "/plots/plot_demres_", metric, ".pdf"),
#'             RMSE = TRUE)
#'
#' @return A plot displaying the chosen metric along a time axis
#' @export
#' @name demres_plot

demres_plot <- function(metric,
                        table,
                        plotname = paste0(getwd(), "/plot_demres_", metric, ".pdf"),
                        rRMSE = FALSE,
                        RMSE = FALSE,
                        MAPE = FALSE) {

  popname = table$popname
  speciesName <- unique(table$popname)
  tableStartYear <- table$timestep

  if(length(grep("_TV", names(table))) == 0){
    stop("The function requires at least one metric
          calculated with the time-varying approach")

  }
  if(length(grep(as.character(metric), names(table))) == 0) {
    stop("The metric you required can not be found in the table")
  }


  if (metric == "dr") {
    name_metric = "Damping ratio"

    #time varying
    table_metric_initvect_TV <-
      table[, grep(paste0(metric, "_TV"), colnames(table))]

    #time constant
    table_metric_initvect_TC <-
      table[, grep(paste0(metric, "_TC"), colnames(table))]

    miny = min(table_metric_initvect_TV, na.rm = TRUE ) - 0.4 * min(table_metric_initvect_TV, na.rm = TRUE )
    maxy = max(table_metric_initvect_TV, na.rm = TRUE )  + 0.4 * max(table_metric_initvect_TV, na.rm = TRUE )

    # Define the vertices of the polygon
    xinit <-
      c(
        min(tableStartYear, na.rm = TRUE ),
        max(tableStartYear, na.rm = TRUE ),
        max(tableStartYear,na.rm = TRUE ),
        min(tableStartYear, na.rm = TRUE )
      )
    yinit <-
      c(
        min(table_metric_initvect_TV, na.rm = TRUE ),
        min(table_metric_initvect_TV, na.rm = TRUE ),
        max(table_metric_initvect_TV, na.rm = TRUE ),
        max(table_metric_initvect_TV, na.rm = TRUE )
      )

    # Create a plot
    pdf(plotname)

    par(mar = c(5, 4, 4, 10), xpd = TRUE)
    plot(
      tableStartYear,
      table_metric_initvect_TV,
      type = "n",
      ylim = c(miny, maxy),
      main = paste0(name_metric, " of ", unique(popname), " population"),
      xlab = "Time step",
      ylab = paste0(name_metric)
    )  # Set limits to make the square more visually clear

    points(
      tableStartYear,
      table_metric_initvect_TV,
      type = "p",
      pch = 19,
      col = "purple",
      ylim = c(miny, maxy)
    )
    lines(
      tableStartYear,
      table_metric_initvect_TC,
      type = "l",
      lwd = 1,
      col = "purple",
      ylim = c(miny, maxy)
    )
    # Draw the polygons
    polygon(xinit, yinit, col = rgb(1, 0, 1, 0.3), border = FALSE)
    #legend
    legend(
      "topright",
      inset = c(-0.4, 0),
      legend = c("Damping ratio"),
      col = c("purple"),
      lty = c(1),
      cex = 0.8,
      title = "Time-Constant",
      box.lty = 0
    )

    #legend
    legend(
      "topright",
      inset = c(-0.4, 0.2),
      legend = c("Damping ratio"),
      col = c("purple"),
      pch = c(19),
      cex = 0.8,
      title = "Time-Varying",
      box.lty = 0,
      title.adj = 0.15
    )

    if(RMSE == TRUE){
      RMSE_dr <- demres_dist(table = table, metric = metric, measure = "RMSE")
      legend(
        "topright",
        inset = c(-0.4, 0.4),
        legend = c(paste0("RMSE dr: ", round(RMSE_dr,3))),
        col = NA,
        pch = NA,
        cex = 0.8,
        title = NA,
        box.lty = 0,
        title.adj = 0.15
      )

    }

    if(rRMSE == TRUE){
      rRMSE_dr <- demres_dist(table = table, metric = metric, measure = "rRMSE")
      legend(
        "topright",
        inset = c(-0.4, 0.4),
        legend = c(paste0("rRMSE dr: ", round(rRMSE_dr,3))),
        col = NA,
        pch = NA,
        cex = 0.8,
        title = NA,
        box.lty = 0,
        title.adj = 0.15
      )    }

    if(MAPE == TRUE){
      MAPE_dr <- demres_dist(table = table, metric = metric, measure = "MAPE")
      legend(
        "topright",
        inset = c(-0.4, 0.4),
        legend = c(paste0("MAPE dr: ", round(MAPE_dr,3))),
        col = NA,
        pch = NA,
        cex = 0.8,
        title = NA,
        box.lty = 0,
        title.adj = 0.15
      )    }


    dev.off()

  }

  else{
    plot_general(metric = metric, table = table, plotname = plotname, RMSE = RMSE, rRMSE = rRMSE, MAPE = MAPE)
  }

}
