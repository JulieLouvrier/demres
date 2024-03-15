plot_general <- function(metric, table, plotname,
                         rRMSE = FALSE,
                         RMSE = FALSE,
                         MAPE = FALSE){
  if(metric == "reac"){
    name_metric = "Reactivity"} 
  else if(metric == "maxamp") {
    name_metric = "Maximum amplification"
  } else if(metric == "maxatt"){
    name_metric = "Maximum attenuation"  
  } else if(metric == "inertia"){
    name_metric = "Inertia"  
  }
  
  popname = table$popname
  speciesName <- unique(table$popname)
  tableStartYear <- table$timestep
  
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
  
  all <- data.frame(table_metric_upr_TV, table_metric_lwr_TV, table_metric_initvect_TV,
                    table_metric_upr_TC, table_metric_lwr_TC, table_metric_initvect_TC)
  
  
  miny = (min(all, na.rm = T) - 0.4 * min(all, na.rm = T))
  maxy = max(all, na.rm = T)
  
  ltydefault = c(1, 1, 1)
  titleTCdefault = "Time-Constant"
  titleTVdefault = "Time-Varying"
  legenddefault = c("Upper bound", "With initial vector", "Lower bound")
  coldefault = c("red", "purple", "blue")
  pchdefault = c(19, 19, 19)
  
  if(length(table_metric_upr_TV) == 0){
    table_metric_upr_TV <- rep(NA, nrow(table))
    maxy <- (max(all, na.rm = T) + 0.4 * max(all, na.rm = T))
    
    if(length(table_metric_lwr_TV) == 0){
      table_metric_lwr_TV <- rep(NA, nrow(table))
      ltydefault = 1
      legenddefault = "With initial vector"
      coldefault = "purple"
      pchdefault = 19
    }
    else {
      if(length(table_metric_initvect_TV) == 0){
        table_metric_initvect_TV <- rep(NA, nrow(table))
        ltydefault = c(1)
        legenddefault = c("Lower bound")
        coldefault = c("blue")
        pchdefault = c(19)
      }
      else {
        ltydefault = c(1, 1)
        legenddefault = c("With initial vector", "Lower bound")
        coldefault = c("purple", "blue")
        pchdefault = c(19, 19)
        
      }
    }
  }
  
  else{
    if(length(table_metric_lwr_TV) == 0) {
      table_metric_lwr_TV <- rep(NA, nrow(table))
      if(length(table_metric_initvect_TV) == 0) {
        table_metric_initvec_TV <- rep(NA, nrow(table))
        ltydefault = c(1)
        legenddefault = c("Upper bound")
        coldefault = c("red")
        pchdefault = c(19)
      }
      else {
        ltydefault = c(1, 1)
        legenddefault = c("Upper bound", "With initial vector")
        coldefault = c("red", "purple")
        pchdefault = c(19, 19)
      }
    }
    
    else{
      if(length(table_metric_initvect_TV) == 0) {
        table_metric_initvec_TV <- rep(NA, nrow(table))
        ltydefault = c(1, 1)
        legenddefault = c("Upper bound", "Lower bound")
        coldefault = c("red", "blue")
        pchdefault = c(19, 19)
      }
      else {
        ltydefault = c(1, 1,1)
        legenddefault = c("Upper bound", "With initial vector", "Lower bound")
        coldefault = c("red","purple", "blue")
        pchdefault = c(19, 19, 19)
      }
      
    }
  }
  
  if(length(table_metric_upr_TC) == 0){
    table_metric_upr_TC <- rep(NA, nrow(table))
  }
  if(length(table_metric_lwr_TC) == 0){
    table_metric_lwr_TC <- rep(NA, nrow(table))
  }
  if(length(table_metric_initvect_TC) == 0){
    table_metric_initvect_TC <- rep(NA, nrow(table))
  }
  
  
  # Define the vertices of the polygon
  xup <-
    c(
      min(tableStartYear,na.rm = T),
      max(tableStartYear, na.rm = T),
      max(tableStartYear,na.rm = T),
      min(tableStartYear, na.rm = T)
    )
  if(length(which(is.na(table_metric_upr_TV))) == nrow(table)) {
    yup = c(NA, NA, NA, NA)
  }
  else{
    yup <- #here
      c(
        min(table_metric_upr_TV, na.rm = T),
        min(table_metric_upr_TV, na.rm = T),
        max(table_metric_upr_TV, na.rm = T),
        max(table_metric_upr_TV, na.rm = T)
      )
  }
  xlow <-
    c(
      min(tableStartYear, na.rm = T),
      max(tableStartYear, na.rm = T),
      max(tableStartYear, na.rm = T),
      min(tableStartYear, na.rm = T)
    )
  
  if(length(which(is.na(table_metric_lwr_TV))) == nrow(table)) {
    ylow = c(NA, NA, NA, NA)
  }
  else{
    ylow <- 
      c(
        min(table_metric_lwr_TV, na.rm = T),
        min(table_metric_lwr_TV, na.rm = T),
        max(table_metric_lwr_TV, na.rm = T),
        max(table_metric_lwr_TV, na.rm = T)
      )
  }
  xinit <-
    c(
      min(tableStartYear, na.rm = T),
      max(tableStartYear, na.rm = T),
      max(tableStartYear, na.rm = T),
      min(tableStartYear, na.rm = T)
    )
  
  if(length(which(is.na(table_metric_initvect_TV))) == nrow(table)) {
    yinit = c(NA, NA, NA, NA)
  }
  else{
    yinit <- 
      c(
        min(table_metric_initvect_TV, na.rm = T),
        min(table_metric_initvect_TV, na.rm = T),
        max(table_metric_initvect_TV, na.rm = T),
        max(table_metric_initvect_TV, na.rm = T)
      )
  }
  # Create a plot
  pdf(plotname)
  par(mar = c(5, 4, 4, 10), xpd = TRUE)
  plot(
    tableStartYear,
    table_metric_upr_TV,
    type = "n",
    ylim = c(miny, maxy),
    main = paste0(name_metric, " of ", unique(popname), " population"),
    xlab = "Time step",
    ylab = paste0(name_metric)
  )  # Set limits to make the square more visually clear
  points(
    tableStartYear,
    table_metric_upr_TV,
    type = "p",
    pch = 19,
    col = "red",
    ylim = c(miny, maxy)
  )
  points(
    tableStartYear,
    table_metric_lwr_TV,
    type = "p",
    pch = 19,
    col = "blue",
    ylim = c(miny, maxy)
  )
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
    table_metric_upr_TC,
    type = "l",
    lwd = 1,
    col = "red",
    ylim = c(miny, maxy)
  )
  lines(
    tableStartYear,
    table_metric_lwr_TC,
    type = "l",
    lwd = 1,
    col = "blue",
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
  polygon(xup, yup, col = rgb(1, 0, 0, 0.3), border = FALSE)
  polygon(xlow, ylow, col = rgb(0, 0, 1, 0.3), border = FALSE)
  polygon(xinit, yinit, col = rgb(1, 0, 1, 0.3), border = FALSE)
  #legend
  
  if(!length(grep("_TC", names(table))) == 0){
    legend(
      "topright",
      inset = c(-0.4, 0),
      legend = legenddefault,
      col = coldefault,
      lty = ltydefault,
      cex = 0.8,
      title = titleTCdefault,
      box.lty = 0
    )
    
  } 
  
  #legend
  legend(
    "topright",
    inset = c(-0.4, 0.2),
    legend = legenddefault,
    col = coldefault,
    pch = pchdefault,
    cex = 0.8,
    title = titleTVdefault,
    box.lty = 0,
    title.adj = 0.15
  )
  
  if(RMSE == TRUE){
    source("R/demres_dist_function.R")
    RMSE <- demres_dist(table = table, metric = metric, measure = "RMSE")
    
    legend(
      "topright",
      inset = c(-0.4, 0.4),
      legend = c(paste0("Upper bound: ", round(RMSE[3],3)),
                 paste0("With initial vector: ", round(RMSE[2],3)),
                 paste0("Lower bound: ", round(RMSE[1],3))),
      col = NA,
      pch = NA,
      cex = 0.8,
      title = "RMSE",
      box.lty = 0,
      title.adj = 0.15
    )
    
  }
  
  if(rRMSE == TRUE){
    source("R/demres_dist_function.R")
    rRMSE <- demres_dist(table = table, metric = metric, measure = "rRMSE")
    legend(
      "topright",
      inset = c(-0.4, 0.4),
      legend = c(paste0("Upper bound: ", round(rRMSE[3],3)),
                 paste0("With initial vector: ", round(rRMSE[2],3)),
                 paste0("Lower bound: ", round(rRMSE[1],3))),
      col = NA,
      pch = NA,
      cex = 0.8,
      title = "rRMSE",
      box.lty = 0,
      title.adj = 0.15
    )
    
      }
  
  if(MAPE == TRUE){
    source("R/demres_dist_function.R")
    MAPE <- demres_dist(table = table, metric = metric, measure = "MAPE")
    legend(
      "topright",
      inset = c(-0.4, 0.4),
      legend = c(paste0("Upper bound: ", round(MAPE[3],3)),
                 paste0("With initial vector: ", round(MAPE[2],3)),
                 paste0("Lower bound: ", round(MAPE[1],3))),
      col = NA,
      pch = NA,
      cex = 0.8,
      title = "MAPE",
      box.lty = 0,
      title.adj = 0.15
    )
    
      }
  
  
  dev.off()
}
