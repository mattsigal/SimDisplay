#' Extract simulation levels
#'
#' \code{get_sim_levels} is a helper function that
#' returns the vector names of the design levels from a \code{SimDesign} object.
#'
#' @param x A \code{data.frame} object, specifically of class \code{SimDesign}.
#'
#' @return A vector of type character.
#' @export
#'
#' @examples
#' \dontrun{
#' #TODO
#' }
#'
#' @seealso \code{\link{SimDisplay}, \link{get_design_levels}}
get_sim_levels <- function(x){
  sim <- attributes(x)$design_names$sim
  sim
}

