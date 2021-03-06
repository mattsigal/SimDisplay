% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimDisplay-package.R
\docType{data}
\name{Brown1974}
\alias{Brown1974}
\title{Brown and Forsythe 1974 Simulation Results}
\format{A data frame with 48 rows and 13 variables}
\usage{
data(Brown1974)
}
\description{
\code{Brown1974} is a a dataset (of class data.frame as well as SimDesign) containing
the standard output from \code{\link{SimDesign}}. This simulation study pertains to the
type I error rates and power for various methods for detecting heterogeneity of variance.
}
\details{
\itemize{
  \item \code{distribution}. The generating distribution: Gaussian, t(4), and chi-square (4)
  \item \code{sample_size}. The sample size conditions in the study (40/40, 10/10, 20/40, and 10/20)
  \item \code{var_ratio}. The variance ratio between the groups (.25, .5, 1, 2, 4)
  \item \code{F}. The EDR (number of cells with p-values less than .05) for the F-test.
  \item \code{Jackknife}. The EDR for the F-test jackknife procedure.
  \item \code{Layard}. The EDR for the Layard procedure.
  \item \code{Levene}. The EDR for the standard Levene procedure.
  \item \code{W10}. The EDR for the Levene test using a ten percent trimmed mean.
  \item \code{W50}. The EDR for the Levene test using the median instead of the mean.
  \item \code{REPLICATIONS}. The number of replications for each cell.
  \item \code{SIM_TIME}. The amount of time each cell took to run.
  \item \code{COMPLETED}. The date and time the simulation was run.
  \item \code{SEED}. The seed values used for each condition.
}
}
\keyword{datasets}
