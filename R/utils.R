# Extract design levels
#
# \code{get_design_levels} is a helper function that
# returns the vector names of the design levels from a \code{SimDesign} object.
#
# @param x A \code{data.frame} object, specifically of class \code{SimDesign}.
#
# @return A vector of type \code{character}.
get_design_levels <- function(x){
  design <- attributes(x)$design_names$design
  design
}

# Extract simulation levels
#
# \code{get_sim_levels} is a helper function that
# returns the vector names of the design levels from a \code{SimDesign} object.
#
# @param x A \code{data.frame} object, specifically of class \code{SimDesign}.
#
# @return A vector of type character.
get_sim_levels <- function(x){
  sim <- attributes(x)$design_names$sim
  sim
}

# sim_design_check
#
# The \code{sim_design_check} function checks an R object to see if it is
# of class \code{SimDesign}. If so, the design and simulation elements are
# extracted and the design variables are converted to factors.
#
# @param x A \code{data.frame} object, specifically of class \code{SimDesign}.
#
# @return A \code{data.frame} object.
sim_design_check <- function(x){
  isSimDesign <- "SimDesign" %in% class(x)

  if (isSimDesign) {
    sim_levels <- get_sim_levels(x)
    des_levels <- get_design_levels(x)
    simDat <- x[c(des_levels, sim_levels)]
    simDat[,des_levels] <- lapply(simDat[,des_levels], as.factor)
  } else stop("Object is not of class SimDesign")

  return(simDat)
}
