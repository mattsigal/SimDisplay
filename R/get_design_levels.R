#' Extract design levels
#'
#' \code{get_design_levels} is a helper function that
#' returns the vector names of the design levels from a \code{SimDesign} object.
#'
#' @param x A \code{data.frame} object, specifically of class \code{SimDesign}.
#'
#' @return A vector of type \code{character}.
#' @export
#'
#' @examples
#' \dontrun{
#' #TODO
#' }
#'
#' @seealso \code{\link{SimDisplay}, \link{get_sim_levels}}
get_design_levels <- function(x){
  design <- attributes(x)$design_names$design
  design
}