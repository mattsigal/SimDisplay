#' Check for and install supplementary packages
#'
#' \code{install_suggests} is a helper function to be used when first
#' installing \code{SimDisplay}. As some of the procedures within this package
#' rely on functions from other packages, this function will check to ensure
#' all required libraries are available.
#'
#' @return NULL
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' install_suggests()
#' }
#'

install_suggests <- function(){
  list.of.packages <- c("methods", "reshape2", "xtable", "grid",
                        "shiny", "ggplot2", "plyr", "tableplot",
                        "RColorBrewer", "shinydashboard", "car",
                        "heplots", "DT", "candisc")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  return("All supplementary packages have been installed.")
}