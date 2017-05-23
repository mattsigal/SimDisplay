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
#' }
#'
summary.SimDesign <- function(x) {
  if (!("SimDesign" %in% class(x)))
    return("This object is not of class SimDesign. See convert_df().")

  groupColumns <- get_design_levels(x)
  dataColumns <- get_sim_levels(x)

  out <- list()

  for (i in 1:length(groupColumns)){
    res <- ddply(x, groupColumns[i], function(x) colMeans(x[dataColumns]))
    out[[i]] <- res
  }

  return(out)
}

