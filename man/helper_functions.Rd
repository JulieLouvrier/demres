% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{helper_functions}
\alias{helper_functions}
\alias{RMSE}
\alias{rRMSE}
\alias{MAPE}
\title{Helper functions for demres_dist function}
\usage{
RMSE(TV, TC)

rRMSE(TV, TC)

MAPE(TV, TC)
}
\arguments{
\item{TV}{A vector containing values of time-varying demographic resilience
for one specific metric}

\item{TC}{A vector containing the value of the time-constant demographic
resilience for the same one specific metric as for TV}
}
\value{
Values of the distance measure
}
\description{
Helper functions for demres_dist function
}
\section{Functions}{
\itemize{
\item \code{RMSE()}: \code{RMSE} Calculates RMSE between the time-varying resilience
metric and the time-constant one

\item \code{rRMSE()}: \code{rRMSE} Calculates rRMSE between the time-varying resilience

\item \code{MAPE()}: \code{MAPE} Calculates MAPE between the time-varying resilience
metric and the time-constant one

}}
\keyword{internal}
