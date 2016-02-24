#' Extract design levels
#'
#' \code{get_design_levels} is a helper function that
#' returns the vector names of the design levels from a SimDesign object.
#'
#' @param x An \code{R} dataframe object, specifically of class \code{SimDesign}.
#'
#' @return A vector of type character.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#' @seealso \code{\link{SimDisplay}, \link{get_sim_levels}}
get_design_levels <- function(x){
  design <- attributes(x)$design_names$design
  design
}

get_sim_levels <- function(x){
  sim <- attributes(x)$design_names$sim
  sim
}

