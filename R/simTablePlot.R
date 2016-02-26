#' \code{simTablePlot} extends \code{ggplot2} to produce table plot displays.
#'
#' @param x A \code{data.frame} object, specifically of class \code{SimDesign}.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' #TODO
#' }
#'
#' @seealso \code{\link{SimDisplay}}, \code{\link{runSimulation}}
simTablePlot <- function(x){
  simDat <- sim_design_check(x)
  p <- ggplot(data=simDat,
              aes(y=sample_size, # devtools::check() warns that these are out of scope. FIXME
                  x=group_size_ratio)) +
    geom_raster(aes(fill = lessthan.05.independent))
  p + facet_grid(. ~ standard_deviation_ratio)
}
