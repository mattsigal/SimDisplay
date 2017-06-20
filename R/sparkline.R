#' Generate sparkline graphic
#'
#' \code{sparkline} takes a vector of type numeric and generates a simple sparkline graphic.
#'
#' @param x A \code{vector} object, pertaining to a row from a tabular display.
#' @param min Numeric input, specifying the minimum value for the y-axis
#' @param max Numeric input, specifying the maximum value for the y-axis
#'
#' @return A \code{ggplot2} graphic object.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' sparkline(Brown1974[1,4:9])
#' sparkline(Brown1974[64,4:9])
#' }
#'
sparkline <- function(x, min = 0, max = 1){

  pdat <- data.frame(Value = as.numeric(x),
                     Method = factor(seq(1, length(as.numeric(x)))))
  plot <- ggplot(pdat, aes(y = Value,
                           x = Method,
                           group = 1)) +
    scale_y_continuous(limits = c(min, max)) +
    geom_line(size = 2) + theme_spark()
  out <- ggplotGrob(plot)
  matches <- grepl("panel", out$layout$name)
  out$layout <- out$layout[matches, , drop = FALSE]
  out$grobs <- out$grobs[matches]
  grid::grid.newpage()
  grid::grid.draw(out)
}
