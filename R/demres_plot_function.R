#' Plot the resulting demographic resilience metrics
#'
#' The function `plot` calls `demres_plot` to visually inspect the resilience metric(s)
#' along a time axis
#' @inheritParams plot_proj
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
#' @param vector a list of numeric vectors or one-column matrices describing the age/stage
#' distribution ('demographic structure') used to calculate a 'case-specific' resilience metric,
#' based on the stage- or age-structure.
#' @param TDvector Boolean. Set to FALSE as default. Specifies whether or not the
#' user wants to obtain a time-dependent list of initial vectors. This vector
#' corresponds to the population stage distribution that is obtained from the projection
#' of the population to the current time step using the specified matrix for each time step
#' @param timeproj Numeric.The number of projection intervals.
#' @param table A dataframe containing all the resilience metrics calculated
#' with the resilience function
#' @name demres_plot
#' @return A plot displaying the chosen metric(s) along a time axis
#' @export
#' @examples
#' # load data
#' data(adeliepenguin)
#'
#' # simulate an initial vector
#' set.seed(125435)
#' vec <- round(runif(2) * 100 )
#' vector_list <- lapply(1:28, function(i) vec)
#'
#' AP_demres <-
#'   resilience(
#'     listA = adeliepenguin,
#'     metrics = "all",
#'     vector = vector_list,
#'     TDvector = FALSE,
#'     popname = "adelie penguin",
#'     verbose = TRUE,
#'     return.N = TRUE,
#'     return.t = TRUE
#'   )
#'
#' # Facet plot with comparison with the other trajectories, ordered by matrices order
#' plot(AP_demres,
#'     listA = adeliepenguin,
#'     vector = vector_list,
#'     timeproj = 5)
#'
#' # Facet plot with comparison with the other trajectories, ordered by decreasing order
#' plot(AP_demres,
#'     listA = adeliepenguin,
#'     vector = vector_list,
#'     timeproj = 5,
#'     sort = TRUE,
#'     compare = TRUE)
#'
#' # One plot to compare them all together
#' plot(AP_demres,
#'     listA = adeliepenguin,
#'     vector = vector_list,
#'     timeproj = 5,
#'     sort = TRUE,
#'     compare = TRUE,
#'     facet = FALSE)
#'
#' # Compare with the standardized value
#'plot(AP_demres,
#'     listA = adeliepenguin,
#'     vector = vector_list,
#'     timeproj = 5,
#'     standard.A = TRUE,
#'     standard.vec = TRUE,
#'     sort = TRUE,
#'     compare = TRUE,
#'     facet = FALSE)


demres_plot <- function(table,
                        listA,
                        vector,
                        TDvector = FALSE,
                        timeproj = 5,
                        standard.A = FALSE,
                        standard.vec = FALSE,
                        facet = NULL,
                        compare = NULL,
                        sort = FALSE,
                        palette = NULL,
                        ...
                        ) {
  if(standard.A){

    listA_stand <- lapply(listA, function(x) {
      M <- x
      eigvals <- eigen(M)$values
      lmax <- which.max(Re(eigvals))
      lambda <- Re(eigvals[lmax])
      A <- M/lambda

      } )
    listA <- listA_stand
  }

 if(standard.vec){
   vec_stand <- lapply(vector, function(x){x/sum(x)})


 }

  if (TDvector) {
    vector <- get_TD_vector(IV = vector[[1]], listA = listA)
  }

 projpopdemres <- mapply(function(A, X) {
     popdemo::project(
       A,
       vector = X,
       standard.A = standard.A,
       time = timeproj
     )
   },
   A = listA,
   X = vector,
   SIMPLIFY = FALSE)

 #now calling the plot_proj function
  pp <- plot_proj(popvec = projpopdemres,
            standard.A = standard.A,
            facet = facet,
            compare = compare,
            sort = sort,
            palette = palette)
  pp

}
