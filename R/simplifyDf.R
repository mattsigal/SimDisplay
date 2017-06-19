#' Simplify SimDesign dataframe
#'
#' \code{simplifyDf} takes a data.frame object of class \code{SimDesign} and
#' returns a data.frame without all of the additional attributes.
#'
#' @param x A \code{SimDesign} \code{data.frame} object.
#'
#' @return A simplified \code{SimDesign} \code{data.frame} object.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' str(Brown1974) # Note the many additional attributes
#' df <- simplifyDf(Brown1974)
#' str(df)
#' }
#'
simplifyDf <- function(x) {
  if (!("SimDesign" %in% class(x)))
    stop("This object is not of class SimDesign. See convert_df().")

  groupColumns <- get_design_levels(x)
  dataColumns <- get_sim_levels(x)
  df <- data.frame(x[,c(groupColumns, dataColumns)])
  return(df)
}
