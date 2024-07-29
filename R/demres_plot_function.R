#' Plot the resulting demographic resilience metrics
#'
#' `demres_plot` provides a plot to visually inspect the resilience metric
#' along a time axis
#' @param table A dataframe containing all the resilience metrics calculated
#' with the resilience function
#' @name demres_plot
#' @return A plot displaying the chosen metric along a time axis
#' @export
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
#'     bluecrane,
#'     metrics = "all",
#'     bounds = TRUE,
#'     vector = Cranevec1,
#'     popname = "blue crane",
#'     time = "both"
#'   )
#'
#' plot(BC_TVTC_demres)

demres_plot <- function(table) {
  sub_names <- grep('[TVTC]', colnames(table), value = TRUE)

  unique_combis_uprlwr <- unlist(strsplit(grep('TV', sub_names, value = TRUE), "_TV"))
  unique_combis_lwr <- unlist(strsplit(unique_combis_uprlwr, "_upr"))
  unique_combis <- unique(unlist(strsplit(unique_combis_lwr, "_lwr")))

  unlist(lapply(unique_combis, FUN = function(x){help_plot(table = table,
                                                           metric = x)}))

}

#' Helper function for the demres_plot function
#'
#' `help_plot` provides a plot to visually inspect the resilience metric
#' along a time axis
#'
#' @param table A dataframe containing all the resilience metrics calculated
#' with the resilience function
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
#' @return A plot displaying the chosen metric along a time axis
#' @export
#' @keywords internal

help_plot <- function(metric,
                        table) {

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
    table_metric_vector_TV <-
      table[, grep(paste0(metric, "_TV"), colnames(table))]

    #time constant
    table_metric_vector_TC <-
      table[, grep(paste0(metric, "_TC"), colnames(table))]

    miny = min(table_metric_vector_TV, na.rm = TRUE ) - 0.4 * min(table_metric_vector_TV, na.rm = TRUE )
    maxy = max(table_metric_vector_TV, na.rm = TRUE )  + 0.4 * max(table_metric_vector_TV, na.rm = TRUE )

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
        min(table_metric_vector_TV, na.rm = TRUE ),
        min(table_metric_vector_TV, na.rm = TRUE ),
        max(table_metric_vector_TV, na.rm = TRUE ),
        max(table_metric_vector_TV, na.rm = TRUE )
      )

    # Create a plot
    graphics::par(mar = c(4,4, 4, 10), #c(5, 4, 4, 10),
                  xpd = TRUE)
    grDevices::dev.new()
    graphics::par(mar = c(4,4, 4, 10), #c(5, 4, 4, 10),
                  xpd = TRUE)
    plot(
      tableStartYear,
      table_metric_vector_TV,
      type = "n",
      ylim = c(miny, maxy),
      main = paste0(name_metric, " of ", unique(popname), " population"),
      xlab = "Time step",
      ylab = paste0(name_metric)
    )  # Set limits to make the square more visually clear

    graphics::points(
      tableStartYear,
      table_metric_vector_TV,
      type = "p",
      pch = 19,
      col = "black",
      ylim = c(miny, maxy)
    )
    graphics::lines(
      tableStartYear,
      table_metric_vector_TC,
      type = "l",
      lwd = 1,
      col = "black",
      ylim = c(miny, maxy)
    )
    # Draw the polygons
    graphics::polygon(xinit, yinit, col = grDevices::rgb(0, 0, 0, 0.3), border = FALSE)
    #legend
    graphics::legend(
      "topright",
      inset = c(-0.4, 0.2),
      legend = c("Damping ratio"),
      col = c("black"),
      lty = c(1),
      cex = 0.8,
      title = "Time-Constant",
      box.lty = 0
    )

    #legend
    graphics::legend(
      "topright",
      inset = c(-0.4, 0.5),
      legend = c("Damping ratio"),
      col = c("black"),
      pch = c(19),
      cex = 0.8,
      title = "Time-Varying",
      box.lty = 0,
      title.adj = 0.15
    )


  }

  else{
    plot_general(metric = metric, table = table)
  }

}
