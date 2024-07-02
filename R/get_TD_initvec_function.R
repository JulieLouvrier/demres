#' Helper function for setting up a list ot´f time dependant initial vectors,
#' starting with one initial vector
#'
#' `get_TD_initvec` provides a list of initial vectors that are projected for
#' each year with each time-varying Matrix, starting with one initial vector
#'
#' @param initvec a one-column matrix describing the age/stage distribution
#'  ('demographic structure') used to calculate a 'case-specific',
#'  stage age structure for the first year. The other initial vectors will be
#'  calculated from this first initial vector.
#' @param listA a list of square, primitive, irreducible, non-negative numeric
#' matrices of any dimension
#' @export
#' @name get_TD_initvec
#' @keywords internal


get_TD_initvec <- function(IV, listA) {
  initvecTD <- list()
  initvecTD[[1]] <- IV
  for (i in 1: length(listA)-1) {
    temp <- popdemo::project(listA[[i]], vector = initvecTD[[1]], time = 1)
    initvecTD[[i + 1]] <- ((temp@vec)[2, ]) / sum(temp@vec[2, ])
  }
  return(initvecTD)
}
