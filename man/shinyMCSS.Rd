% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyMCSS.R
\name{shinyMCSS}
\alias{shinyMCSS}
\title{Generate a basic interactive applet for a SimDesign dataset}
\usage{
shinyMCSS(dataframe = NULL, percents = FALSE, ndigits = 2)
}
\arguments{
\item{dataframe}{an optional \code{dataframe} object of class \code{SimDesign}.
If omitted, the app will load \code{data(Brown1974)}.}

\item{percents}{\code{Boolean}, indicating whether to display numeric simulation
results as percentages.}

\item{ndigits}{\code{Integer}. If \code{percents} is true, how many digits should be printed?}
}
\description{
\code{shinyMCSS} generates an interactive applet for the presentation and
analysis of a \code{SimDesign} dataframe. The backend was created using
the \code{shiny} package. The default applet is relatively straightforward,
but allows for the exporting of the relavant server and ui files to allow
further customization.
}
\examples{
\dontrun{
# Load default internal dataset:
shinyMCSS()

# Example of passing a SimDesign dataset from the current global environment:
data(Brown1974)
shinyMCSS(Brown1974)
}
}
\references{
Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
\url{http://www.tandfonline.com/doi/full/10.1080/10691898.2016.1246953}
}
\seealso{
\code{\link{Brown1974}}
}
