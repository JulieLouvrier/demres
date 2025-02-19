#' Helper function for the demres_plot function
#'
#' `plot_general` provides a plot to visually inspect the resilience metric
#' along a time axis
#'
#' @param table A dataframe containing all the resilience metrics calculated
#' with the resilience function
#' @param metric "convt": the time to convergence of a
#'                 matrix projection model.\cr
#'                 "dr": the damping ratio of a given
#'                 matrix projection model.\cr
#'                 "inertia": population inertia for a
#'                 matrix projection model.\cr
#'                 "maxamp": maximal amplification for a
#'                 matrix projection model.\cr
#'                 "maxatt": maximal attenuation for a
#'                 matrix projection model.\cr
#'                 "reac": reactivity: first time step amplification
#'                 and first time step attenuation for a matrix
#'                 projection model.\cr
#' @return A plot displaying the chosen metric along a time axis
#' @export
#' @name plot_general
#' @keywords internal
#'
#' ju: add par(xpd=TRUE) for the legend on the right and then set par(xpd = FALSE)
#' for resetting plotting options, example:
#' legend("topright", legend = c("Series 1"), col = "blue", pch = 1,
#' xpd = TRUE, inset = c(-0.2, 0))
#' with larger coordinates for the margins

plot_general <- function(metric, table){
  if(metric == "reac"){
    name_metric = "Reactivity"}
  else if(metric == "maxamp") {
    name_metric = "Maximum amplification"
  } else if(metric == "maxatt"){
    name_metric = "Maximum attenuation"
  } else if(metric == "inertia"){
    name_metric = "Inertia"
  } else if(metric == "convt") {
    name_metric = "Convergence time"
  }

  popname = table$popname
  speciesName <- unique(table$popname)
  tableStartYear <- table$timestep

  #time varying

  table_metric_upr_TV <-
    table[, grep(paste0(metric, "_upr_TV"), colnames(table))]
  table_metric_lwr_TV <-
    table[, grep(paste0(metric, "_lwr_TV"), colnames(table))]
  table_metric_vector_TV <-
    table[, grep(paste0(metric, "_TV"), colnames(table))]

  #time constant
  table_metric_upr_TC <-
    table[, grep(paste0(metric, "_upr_TC"), colnames(table))]
  table_metric_lwr_TC <-
    table[, grep(paste0(metric, "_lwr_TC"), colnames(table))]
  table_metric_vector_TC <-
    table[, grep(paste0(metric, "_TC"), colnames(table))]

  all <- data.frame(table_metric_upr_TV, table_metric_lwr_TV, table_metric_vector_TV,
                    table_metric_upr_TC, table_metric_lwr_TC, table_metric_vector_TC)

  miny = (min(all, na.rm = TRUE) - 0.4 * min(all, na.rm = TRUE))
  maxy = max(all, na.rm = TRUE)

  ltydefault = c(1, 1, 1)
  titleTCdefault = "Time-Constant"
  titleTVdefault = "Time-Varying"
  legenddefault = c("Upper bound", "With initial vector", "Lower bound")
  coldefault = c("#CC79A7", "black", "#009E73")
  pchdefault = c(19, 19, 19)

  if(length(table_metric_upr_TV) == 0){
    table_metric_upr_TV <- rep(NA, nrow(table))
    maxy <- (max(all, na.rm = TRUE) + 0.4 * max(all, na.rm = TRUE))

    if(length(table_metric_lwr_TV) == 0){
      table_metric_lwr_TV <- rep(NA, nrow(table))
      ltydefault = 1
      legenddefault = "With initial vector"
      coldefault = "black"
      pchdefault = 19
    }
    else {
      if(length(table_metric_vector_TV) == 0){
        table_metric_vector_TV <- rep(NA, nrow(table))
        ltydefault = c(1)
        legenddefault = c("Lower bound")
        coldefault = c("#009E73")
        pchdefault = c(19)
      }
      else {
        ltydefault = c(1, 1)
        legenddefault = c("With initial vector", "Lower bound")
        coldefault = c("black", "#009E73")
        pchdefault = c(19, 19)

      }
    }
  }

  else if(length(table_metric_lwr_TV) == 0) {
      table_metric_lwr_TV <- rep(NA, nrow(table))
      maxy <- (max(all, na.rm = TRUE) + 0.4 * max(all, na.rm = TRUE))

      if(length(table_metric_upr_TV) == 0){
        table_metric_upr_TV <- rep(NA, nrow(table))
        ltydefault = 1
        legenddefault = "With initial vector"
        coldefault = "black"
        pchdefault = 19
      }
      else {
        if(length(table_metric_vector_TV) == 0){
          table_metric_vector_TV <- rep(NA, nrow(table))
          ltydefault = c(1)
          legenddefault = c("Upper bound")
          coldefault = c("#CC79A7")
          pchdefault = c(19)
        }
        else {
          ltydefault = c(1, 1)
          legenddefault = c("With initial vector", "Upper bound")
          coldefault = c("black", "#CC79A7")
          pchdefault = c(19, 19)

        }
      }
    }

    else if(length(table_metric_vector_TV) == 0) {
        table_metric_vector_TV <- rep(NA, nrow(table))
        ltydefault = c(1, 1)
        legenddefault = c("Upper bound", "Lower bound")
        coldefault = c("#CC79A7", "#009E73")
        pchdefault = c(19, 19)
    }
      else {
        ltydefault = c(1, 1,1)
        legenddefault = c("Upper bound", "With initial vector", "Lower bound")
        coldefault = c("#CC79A7","black", "#009E73")
        pchdefault = c(19, 19, 19)
      }


  if(length(table_metric_upr_TC) == 0){
    table_metric_upr_TC <- rep(NA, nrow(table))
  }
  if(length(table_metric_lwr_TC) == 0){
    table_metric_lwr_TC <- rep(NA, nrow(table))
  }
  if(length(table_metric_vector_TC) == 0){
    table_metric_vector_TC <- rep(NA, nrow(table))
  }


  # Define the vertices of the polygon
  xup <-
    c(
      min(tableStartYear,na.rm = TRUE),
      max(tableStartYear, na.rm = TRUE),
      max(tableStartYear,na.rm = TRUE),
      min(tableStartYear, na.rm = TRUE)
    )
  if(length(which(is.na(table_metric_upr_TV))) == nrow(table) | length(table_metric_upr_TV) ==0) {
    yup = c(NA, NA, NA, NA)
  }
  else{
    yup <- #here
      c(
        min(table_metric_upr_TV, na.rm = TRUE),
        min(table_metric_upr_TV, na.rm = TRUE),
        max(table_metric_upr_TV, na.rm = TRUE),
        max(table_metric_upr_TV, na.rm = TRUE)
      )
  }
  xlow <-
    c(
      min(tableStartYear, na.rm = TRUE),
      max(tableStartYear, na.rm = TRUE),
      max(tableStartYear, na.rm = TRUE),
      min(tableStartYear, na.rm = TRUE)
    )

  if(length(which(is.na(table_metric_lwr_TV))) == nrow(table) | length(table_metric_lwr_TV) ==0) {
    ylow = c(NA, NA, NA, NA)
  }
  else{
    ylow <-
      c(
        min(table_metric_lwr_TV, na.rm = TRUE),
        min(table_metric_lwr_TV, na.rm = TRUE),
        max(table_metric_lwr_TV, na.rm = TRUE),
        max(table_metric_lwr_TV, na.rm = TRUE)
      )
  }
  xinit <-
    c(
      min(tableStartYear, na.rm = TRUE),
      max(tableStartYear, na.rm = TRUE),
      max(tableStartYear, na.rm = TRUE),
      min(tableStartYear, na.rm = TRUE)
    )

  if(length(which(is.na(table_metric_vector_TV))) == nrow(table) | length(table_metric_vector_TV) ==0) {
    yinit = c(NA, NA, NA, NA)
  }
  else{
    yinit <-
      c(
        min(table_metric_vector_TV, na.rm = TRUE),
        min(table_metric_vector_TV, na.rm = TRUE),
        max(table_metric_vector_TV, na.rm = TRUE),
        max(table_metric_vector_TV, na.rm = TRUE)
      )
  }
  # Create a plot
  oldpar <- graphics::par(mar = c(4, 4, 4, 10), #c(5, 4, 4, 10),
                          xpd = TRUE)
  on.exit(graphics::par(oldpar)) ## restore original parameters once the function has ran
  #grDevices::dev.new()
  graphics::par(mar = c(4, 4, 4, 10), #c(5, 4, 4, 10),
                xpd = TRUE)
  plot(
    tableStartYear,
    table_metric_upr_TV,
    type = "n",
    ylim = c(miny, maxy),
    main = paste0(name_metric, " of ", unique(popname), " population"),
    xlab = "Time step",
    ylab = paste0(name_metric)
  )  # Set limits to make the square more visually clear
  graphics::points(
    tableStartYear,
    table_metric_upr_TV,
    type = "p",
    pch = 19,
    col = "#CC79A7",
    ylim = c(miny, maxy)
  )
  graphics::points(
    tableStartYear,
    table_metric_lwr_TV,
    type = "p",
    pch = 19,
    col = "#009E73",
    ylim = c(miny, maxy)
  )
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
    table_metric_upr_TC,
    type = "l",
    lwd = 1,
    col = "#CC79A7",
    ylim = c(miny, maxy)
  )
  graphics::lines(
    tableStartYear,
    table_metric_lwr_TC,
    type = "l",
    lwd = 1,
    col = "#009E73",
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
  graphics::polygon(xup, yup, col = grDevices::rgb(0.80, 0.47, 0.65, 0.3), border = FALSE)
  graphics::polygon(xlow, ylow, col = grDevices::rgb(0, 0.61, 0.45, 0.3), border = FALSE)
  graphics::polygon(xinit, yinit, col = grDevices::rgb(0, 0, 0, 0.3), border = FALSE)
  #legend

  if(!length(grep("_TC", names(table))) == 0){
    graphics::legend(
      "topright",
      inset = c(-0.6, 0),
      legend = legenddefault,
      col = coldefault,
      lty = ltydefault,
      cex = 0.8,
      title = titleTCdefault,
      box.lty = 0
    )

  }

  #legend
  graphics::legend(
    "topright",
    inset = c(-0.6, 0.5),
    legend = legenddefault,
    col = coldefault,
    pch = pchdefault,
    cex = 0.8,
    title = titleTVdefault,
    box.lty = 0,
    title.adj = 0.15
  )

  grDevices::dev.flush()

}
