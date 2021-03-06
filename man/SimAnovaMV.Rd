% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimAnovaMV.R
\name{SimAnovaMV}
\alias{SimAnovaMV}
\title{Decomposing multivariate simulation results into ANOVA-based effects sizes}
\usage{
SimAnovaMV(data, terms, responses)
}
\arguments{
\item{data}{A \code{data.frame} object, specifically of class \code{SimDesign}.}

\item{terms}{A \code{character} vector indicating the design factors to summarise.}

\item{responses}{A \code{character} vector indicating the response variables produced in the simulation.}
}
\value{
A \code{data.frame} or \code{list} object.
}
\description{
This function is an extension of \code{SimAnova}, which takes an output object from
a simulation, as well as a vector for the design factors and outcome measures. If
there is only one design factor, then the result is a dataframe with rows pertaining
to each outcome measure. If there is more than one design factor, this function
automatically conducts a full-factorial design, and outputs a list with elements
corresponding to each main and interaction effect.
}
\examples{
\dontrun{
data(Brown1974) # 3 design factors, 6 response variables
TypeI <- subset(Brown1974, var_ratio == 1)
Power <- subset(Brown1974, var_ratio != 1)
tvals <- c("distribution")
rvals <- c("F", "Jacknife", "Layard", "Levene", "W10", "W50")
SimAnovaMV(data = TypeI, terms = tvals, responses = rvals)
SimAnovaMV(data = Power, terms = tvals, responses = rvals)

data(tsimresults) # 3 design factors, 2 response variables
tvals <- c("group_size_ratio", "standard_deviation_ratio")
rvals <- c("lessthan.05.welch", "lessthan.05.independent")
SimAnovaMV(data = tsimresults, terms = tvals, responses = rvals)
}

}
\seealso{
\code{\link{SimAnova}}
}
