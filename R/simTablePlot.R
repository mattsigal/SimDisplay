#' \code{simTablePlot} extends ggplot2 to produce table plot displays.
#'
#' @param x An \code{R} dataframe object, specifically of class \code{SimDesign}.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#' @seealso \code{\link{SimDisplay}}

simTablePlot <- function(x){
  library(ggplot2)
  simDat <- sim_design_check(x)
  p <- ggplot(data=simDat,
              aes(y=sample_size,
                  x=group_size_ratio)) +
    geom_raster(aes(fill = lessthan.05.independent))
  p + facet_grid(. ~ standard_deviation_ratio)
}
