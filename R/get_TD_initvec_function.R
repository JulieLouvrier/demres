#' Helper function for setting up a list of time-dependent initial vectors,
#' starting with the first initial vector
#'
#' `get_TD_vector` provides a list of initial vectors that are used for each time
#' step together with each time-varying population matrix to project population
#' dynamics over time.
#'
#' @param IV a one-column matrix describing the age/stage distribution
#'  ('demographic structure') used to calculate a 'case-specific',
#'  stage/ age structure for the first year. The other initial vectors will be
#'  calculated from this first initial vector.
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
#' @export
#' @name get_TD_vector
#' @keywords internal

get_TD_vector <- function(IV, listA) {
  vectorTD <- list()
  vectorTD[[1]] <- IV
  lengthA <- length(listA)
  if (lengthA == 0) {
    stop("listA has length 0")
  } else if (lengthA == 1) {
    return(vectorTD)
  } else {
    for (i in seq_len(lengthA - 1)) {
      temp <- popdemo::project(listA[[i]], vector = vectorTD[[i]], time = 1)
      vectorTD[[i + 1]] <- as.numeric(((temp@vec)[2, ]) / sum(temp@vec[2, ]))
    }
  }
  return(vectorTD)
}
