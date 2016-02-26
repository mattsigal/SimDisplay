# SimDisplay-package
#'
#' The SimDisplay package provides a variety of methods for visualizing
#' results from complex tables. It has been specifically designed to
#' summarise Monte Carlo Simulation Study objects from \code{\link{SimDesign}} (Chalmers, 2016).
#'
#' @section SimDisplay functions:
#' So far, this package implements the following functions: \code{tableplot}.
#'
#' @name SimDisplay-package
#' @aliases SimDisplay
#' @title SimDisplay - Methods for visualizing complex tables.
#' @author Matthew Sigal \email{matthewsigal@@gmail.com}
#' @import shiny methods ggplot2 SimDesign
#' @docType package
#' @keywords package
NULL

#' Sample Simulation Results
#'
#' \code{tsimresults} is a a dataset (of class data.frame as well as SimDesign) containing
#' the standard output from \code{\link{SimDesign}}.
#' For details on how this simulation was run, please refer to \code{\link{runSimulation}}.
#' The variables in the \code{data.frame} are:
#'
#' \itemize{
#'   \item \code{sample_size}. The sample size conditions in the study (30, 60, 90, 120)
#'   \item \code{group_size_ratio}. The difference in sample size between the groups (.5, 1, 2)
#'   \item \code{standard_deviation_ratio}. The difference in standard deviation between the groups (1, 4, 8)
#'   \item \code{lessthan.05.welch}. The number of cells with p-values less than .05, using the Welch correction.
#'   \item \code{lessthan.05.independent}. The number of cells with p-values less than .05, not using the Welch correction.
#'   \item \code{REPLICATIONS}. The number of replications for each cell.
#'   \item \code{SIM_TIME}. The amount of time each cell took to run.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name tsimresults
#' @usage data(tsimresults)
#' @format A data frame with 36 rows and 7 variables
NULL