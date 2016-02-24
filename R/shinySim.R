#' shinySim
#' \code{shinySim} is a function that launches an interactive shiny interface
#' specifically designed for the analysis of simulation study designs.
#'
#' @param x An \code{R} dataframe object, specifically of class \code{SimDesign}.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#' @seealso \code{\link{SimDisplay}}

shinySim <- function(x){
  library(shiny)
  simDat <- sim_design_check(x)

  return(simDat) # test to see if return is as expected
}