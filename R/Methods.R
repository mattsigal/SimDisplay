#' Summary method for SimDesign objects
#'
#' \code{summary.SimDesign} is a default method for summarizing a
#' \code{data.frame} of class \code{SimDesign}.
#'
#' @param x A \code{data.frame} object, of class \code{SimDesign}.
#'
#' @return NULL
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' summary(Brown1974)
#' type1 <- subset(Brown1974, var_ratio == 1)
#' power <- subset(Brown1974, var_ratio != 1)
#'
#' summary(type1)
#' summary(power)
#' }
#'
summary.SimDesign <- function(x, digits = 3, percent = FALSE) {
  if (!("SimDesign" %in% class(x)))
    return("This object is not of class SimDesign. See convert_df().")

  groupColumns <- SimDisplay:::get_design_levels(x)
  dataColumns <- SimDisplay:::get_sim_levels(x)

  out <- list()
  for (i in 1:length(groupColumns)){
    res <- ddply(x, groupColumns[i],
                 function(x) roundSim(colMeans(x[dataColumns]),
                                       digits = digits,
                                       percent = percent))
    out[[i]] <- res
  }
  names(out) <- groupColumns
  return(out)
}

