% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demres_dist_function.R
\name{demres_dist}
\alias{demres_dist}
\title{Calculates distance between the time-varying resilience metric and the time-
constant one}
\usage{
demres_dist(table)
}
\arguments{
\item{table}{A dataframe containing all the resilience metrics calculated
with the resilience function}
}
\value{
A dataframe displaying the distance measures for the metrics that are present in the table
}
\description{
\code{summary} Calculates distance between the time-varying resilience
metric and the time-constant one:
"RMSE": sqrt(mean((TV-TC)^2))
with TV: the time-Varying resilience metric and TC the time constant one
"rRMSE": sqrt(mean((TV-TC)^2)) / sd(TV)
with TV: the time-Varying resilience metric and TC the time constant
"MAPE": mean(abs(TV - TC))
with TV: the time-Varying resilience metric and TC the time constant
}
\examples{
# load data
data(bluecrane)

# simulate an initial vector
set.seed(1234)
Cranevec1 <- runif(5)
Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vec to sum to 1

BC_TVTC_demres <-
  resilience(
    listA = bluecrane,
    metrics = "all",
    bounds = TRUE,
    vector = Cranevec1,
    TDvector = FALSE,
    popname = "blue crane",
    time = "both",
    verbose = TRUE
  )

dist_BC <- summary(BC_TVTC_demres)

}
