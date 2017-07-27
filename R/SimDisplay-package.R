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
#' @import stats shiny methods ggplot2 SimDesign plyr reshape2 xtable
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

#' Brown and Forsythe 1974 Simulation Results
#'
#' \code{Brown1974} is a a dataset (of class data.frame as well as SimDesign) containing
#' the standard output from \code{\link{SimDesign}}. This simulation study pertains to the
#' type I error rates and power for various methods for detecting heterogeneity of variance.
#'
#' \itemize{
#'   \item \code{distribution}. The generating distribution: Gaussian, t(4), and chi-square (4)
#'   \item \code{sample_size}. The sample size conditions in the study (40/40, 10/10, 20/40, and 10/20)
#'   \item \code{var_ratio}. The variance ratio between the groups (.25, .5, 1, 2, 4)
#'   \item \code{F}. The EDR (number of cells with p-values less than .05) for the F-test.
#'   \item \code{Jackknife}. The EDR for the F-test jackknife procedure.
#'   \item \code{Layard}. The EDR for the Layard procedure.
#'   \item \code{Levene}. The EDR for the standard Levene procedure.
#'   \item \code{W10}. The EDR for the Levene test using a ten percent trimmed mean.
#'   \item \code{W50}. The EDR for the Levene test using the median instead of the mean.
#'   \item \code{REPLICATIONS}. The number of replications for each cell.
#'   \item \code{SIM_TIME}. The amount of time each cell took to run.
#'   \item \code{COMPLETED}. The date and time the simulation was run.
#'   \item \code{SEED}. The seed values used for each condition.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name Brown1974
#' @usage data(Brown1974)
#' @format A data frame with 48 rows and 13 variables
NULL

#' Hallgren 2013 Simulation Results
#'
#' \code{Hallgren2013} is a a dataset (of class data.frame as well as SimDesign) containing
#' the standard output from \code{\link{SimDesign}}. This simulation study pertains to the
#' testing mediation in properly and improperly specified models.
#'
#' \itemize{
#'   \item \code{N}. Sample size was manipulated at 3 levels: 30, 100, 300.
#'   \item \code{a}. Coefficients for regression path a was manipulated at three levels: -0.3, 0.0, 0.3
#'   \item \code{b}. Coefficients for regression path b was manipulated at three levels: -0.3, 0.0, 0.3
#'   \item \code{cp}. Coefficients for regression path c' was manipulated at three levels: -0.2, 0.0, 0.2.
#'   \item \code{XMY.p}. The EDR for the properly specified model.
#'   \item \code{XYM.p}. The EDR for the improperly specified model.
#'   \item \code{REPLICATIONS}. The number of replications for each cell.
#'   \item \code{SIM_TIME}. The amount of time each cell took to run.
#'   \item \code{COMPLETED}. The date and time the simulation was run.
#'   \item \code{SEED}. The seed values used for each condition.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name Hallgren2013
#' @usage data(Hallgren2013)
#' @format A data frame with 81 rows and 10 variables
NULL
