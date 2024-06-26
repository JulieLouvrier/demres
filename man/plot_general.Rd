% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_general.R
\name{plot_general}
\alias{plot_general}
\title{Helper function for the demres_plot function}
\usage{
plot_general(
  metric,
  table,
  plotname,
  rRMSE = FALSE,
  RMSE = FALSE,
  MAPE = FALSE
)
}
\arguments{
\item{table}{A dataframe containing all the resilience metrics calculated
with the demres function}

\item{metric:}{"reac": Reactivity: first-timestep amplification
and first-timestep attenuation for a population matrix
projection model.
"inertia": Population inertia for a population
matrix projection model.
"dr": Damping ratio of a given population
matrix projection model.
"maxamp": Maximal amplification for a population
matrix projection model.
"maxatt": Maximal attenuation for a population
matrix projection model.}

\item{plotname:}{name of the plot with directory. By default:
plotname = paste0(getwd(), "/plot_demres_", metric, ".pdf")}
}
\value{
A plot displaying the chosen metric along a time axis
}
\description{
\code{plot_general} provides a plot to visually inspect the resilience metric
along a time axis
}
\keyword{internal}
