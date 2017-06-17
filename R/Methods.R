#' Summary method for SimDesign objects
#'
#' \code{summary.SimDesign} is a default method for summarizing a
#' \code{data.frame} of class \code{SimDesign}.
#'
#' @param object A \code{data.frame} object, of class \code{SimDesign}
#' @param digits TODO
#' @param percent TODO
#' @param ... TODO
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
summary.SimDesign <- function(object, digits = 3, percent = FALSE, ...) {
  if (!("SimDesign" %in% class(object)))
    stop("This object is not of class SimDesign. See convert_df().")

  groupColumns <- get_design_levels(object)
  dataColumns <- get_sim_levels(object)

  out <- vector('list', length(groupColumns))
  for (i in 1:length(groupColumns)){
    res <- plyr::ddply(object, groupColumns[i],
                 function(x) round_sim(colMeans(object[dataColumns]),
                                       digits = digits,
                                       percent = percent))
    out[[i]] <- res
  }
  names(out) <- groupColumns
  return(out)
}

