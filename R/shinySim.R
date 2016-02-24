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
  fdat <- x
  isSimDesign <- "SimDesign" %in% class(fdat)

  if (isSimDesign) {
    sim_levels <- get_sim_levels(fdat)
    des_levels <- get_design_levels(fdat)
    simDat <- fdat[c(des_levels, sim_levels)]
    simDat[,des_levels] <- lapply(simDat[,des_levels], as.factor)
  }
  return(simDat) # test to see if return is as expected
}