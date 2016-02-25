# SimDisplay-package
#'
#' The SimDisplay package provides a variety of methods for visualizing
#' results from complex tables. It has been specifically designed to
#' summarise Monte Carlo Simulation Study objects from SimDesign (Chalmers, 2016).
#'
#' @section SimDisplay functions:
#' So far, this package implements the following functions: \code{tableplot}.
#'
#' @name SimDisplay-package
#' @title SimDisplay - Methods for visualizing complex tables.
#' @author Matthew Sigal \email{matthewsigal@@gmail.com}
#' @docType package
#' @keywords package
NULL

#' Sample Simulation Results
#'
#' \code{tsimresults} is a a dataset (of class data.frame as well as SimDesign) containing
#' the standard output from \code{SimDesign}.
#' For details on how this simulation was run, please refer to \code{?SimDesign::runSimulation}.
#' The variables in the dataframe are:
#'
#' \itemize{
#'   \item sample_size. The sample size conditions in the study (30, 60, 90, 120)
#'   \item group_size_ratio. The difference in sample size between the groups (.5, 1, 2)
#'   \item standard_deviation_ratio. The difference in standard deviation between the groups (1, 4, 8)
#'   \item lessthan.05.welch. The number of cells with p-values less than .05, using the Welch correction.
#'   \item lessthan.05.independent. The number of cells with p-values less than .05, not using the Welch correction.
#'   \item REPLICATIONS. The number of replications for each cell.
#'   \item SIM_TIME. The amount of time each cell took to run.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tsimresults
#' @usage data(tsimresults)
#' @format A data frame with 36 rows and 7 variables
NULL