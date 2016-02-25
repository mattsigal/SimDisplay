#' Convert to SimDesign Object
#'
#' \code{convert_df} is a helper function that
#' can convert a dataframe into a SimDesign object.
#'
#' @param x An \code{R} dataframe object, specifically with simulation design
#' variables specified as factors, and simulation results as numeric.
#'
#' @return An \code{R} dataframe object, of class SimDesign and data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#' @seealso \code{\link{SimDisplay}}
convert_df <- function(x){
  x <- as.data.frame(x)
  is.fact <- sapply(x, is.factor)
  attributes(x)$design_names$design <- names(is.fact[is.fact == TRUE])

  is.sim <- sapply(x, is.numeric)
  attributes(x)$design_names$sim <- names(is.sim[is.sim == TRUE])

  class(x) <- c("SimDesign", "data.frame")
  return(x)
}
