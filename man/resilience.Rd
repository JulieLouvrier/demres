% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resilience_function.R
\name{resilience}
\alias{resilience}
\title{Provides time-varying and time-constant resilience metrics for animal
populations based on a list of matrix population models}
\usage{
resilience(
  listA,
  metrics = "all",
  bounds = FALSE,
  vector = "n",
  TDvector = FALSE,
  popname = NULL,
  time = "both",
  verbose = TRUE,
  accuracy = 0.01,
  iterations = 1e+05
)
}
\arguments{
\item{listA}{a list of square, primitive, irreducible, non-negative numeric
matrices of any dimension}

\item{metrics}{"reac": Calculates reactivity: first-timestep amplification
and first-timestep attenuation for a population matrix
projection model.
"inertia": Calculates population inertia for a population
matrix projection model.
"dr": Calculate the damping ratio of a given population
matrix projection model.
"maxamp": Calculate maximal amplification for a population
matrix projection model.
"maxatt": Calculate maximal attenuation for a population
matrix projection model.
"convt": Calculate the time to convergence of a population
matrix projection model from the model projection
"all": all of the above metrics are provided}

\item{bounds}{(optional) set to FALSE as default. If TRUE, specifies whether the upper and  lower
bound should be calculated
if vector is not specified, the function provides metrics in their upper and
lower bound, calculated based on the stage-biased vector
if vector is specified, the function provides also the metrics calculated
based on the inital vector}

\item{vector}{a list of numeric vectors or one-column matrices describing the age/stage
distribution ('demographic structure') used to calculate a 'case-specific', stage age structure
metric}

\item{TDvector}{set to FALSE as default. Specifies whether or not the user wants to get a Time-Dependant list of initial vectors,
corresponding to the population stage distribution projection for each year with each matrix of each year.}

\item{popname}{a character string describing the name of the population}

\item{time}{set to "both" as default. A character string: "constant", "varying" or "both"
"constant" : if the metrics are to be calculated over the whole
study period
"varying": if the metrics are to be calculated for each time step}

\item{verbose}{set to TRUE as default a boolean indicating if messages about failure to compute particular metrics should be displayed or not (default = TRUE)}

\item{accuracy}{the accuracy with which to determine convergence on asymptotic growth,
expressed as a proportion. Set to 0.01 by default.}

\item{iterations}{the maximum number of iterations of the model before the code breaks. For slowly-converging models
and/or high specified convergence accuracy, this may need to be increased.
Set to 1e+05 by default.}
}
\value{
An object of class "resil", which is a dataframe
containing all the resilience metrics.
}
\description{
\code{resilience} calculates resilience metrics of a population based
on a list of matrix population models
}
\details{
This function works with to a list of matrices or just one matrix
and returns either time-varying metrics or time-constant metrics
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
    time = "varying",
    verbose = TRUE
  )

}
