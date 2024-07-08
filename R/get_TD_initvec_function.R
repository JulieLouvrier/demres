#' Helper function for setting up a list of time dependant initial vectors,
#' starting with one initial vector
#'
#' `get_TD_vector` provides a list of initial vectors that are projected for
#' each year with each time-varying Matrix, starting with one initial vector
#'
#' @param IV a one-column matrix describing the age/stage distribution
#'  ('demographic structure') used to calculate a 'case-specific',
#'  stage age structure for the first year. The other initial vectors will be
#'  calculated from this first initial vector.
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
#' @export
#' @name get_TD_vector
#' @keywords internal

get_TD_vector <- function(IV, listA) {
  vectorTD <- list()
  vectorTD[[1]] <- IV
  for (i in 1:(length(listA)-1)) {
    temp <- popdemo::project(listA[[i]], vector = vectorTD[[i]], time = 1)
    vectorTD[[i + 1]] <- as.numeric(((temp@vec)[2, ]) / sum(temp@vec[2, ]))
  }
  return(vectorTD)
}
