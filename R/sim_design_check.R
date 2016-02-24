#' sim_design_check
#' The \code{sim_design_check} function checks an R object to see if it is
#' of class SimDesign. If so, the design and simulation elements are
#' extracted and the design variables are converted to factors.
#'
#' @param x An \code{R} dataframe object, specifically of class \code{SimDesign}.
#'
#' @return An R dataframe object.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#' @seealso \code{\link{SimDisplay}}
#'

sim_design_check <- function(x){
  isSimDesign <- "SimDesign" %in% class(x)

  if (isSimDesign) {
    sim_levels <- get_sim_levels(x)
    des_levels <- get_design_levels(x)
    simDat <- x[c(des_levels, sim_levels)]
    simDat[,des_levels] <- lapply(simDat[,des_levels], as.factor)
  } else return("Object does not have class SimDesign")

  return(simDat)
}