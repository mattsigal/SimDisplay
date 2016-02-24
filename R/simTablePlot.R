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
  simDat <- sim_design_check(x)

  ggplot(data = dat,
         aes(x = Var2,
             y = Var1)) +
  #geom_raster(aes(fill = value)) +
  scale_fill_grey(name = "",
                  labels = c("Present","Missing")) +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
  labs(x = "Variables in Dataset",
       y = "Rows / observations")
}