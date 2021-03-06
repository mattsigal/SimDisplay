#' Summary method for SimDesign objects
#'
#' \code{summary.SimDesign} is a default method for summarizing a
#' \code{data.frame} of class \code{SimDesign}.
#'
#' @param object A \code{data.frame} object, of class \code{SimDesign}
#' @param digits Number of digits to display, defaults to 3.
#' @param percent A \code{boolean} indicating if results be converted to percentages.
#' @param ... Optional arguments not currently utilized.
#'
#' @return NULL
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' summary(Brown1974)
#' type1 <- subset(Brown1974, var_ratio == 1)
#' power <- subset(Brown1974, var_ratio != 1)
#'
#' summary(type1)
#' summary(power)
#' }
#'
summary.SimDesign <- function(object, digits = 3, percent = FALSE, ...) {
  if (!("SimDesign" %in% class(object)))
    stop("This object is not of class SimDesign. See convertDf().")

  groupColumns <- get_design_levels(object)
  dataColumns <- get_sim_levels(object)
  df <- simplifyDf(object)
  out <- vector('list', length(groupColumns))

  for (i in 1:length(groupColumns)){
    res <- plyr::ddply(df, groupColumns[i],
                       function(df) roundSim(colMeans(df[dataColumns]),
                                             digits = digits,
                                             percent = percent))
    out[[i]] <- res
  }
  names(out) <- groupColumns
  return(out)
}


