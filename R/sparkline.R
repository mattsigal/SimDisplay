#' Generate sparkline graphic
#'
#' \code{sparkline} takes a vector of type numeric and generates a simple sparkline graphic.
#'
#' @param x A \code{vector} object, pertaining to a row from a tabular display.
#'
#' @return A \code{ggplot2} graphic object.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' }
#'
sparkline <- function(x){
  pdat <- data.frame(Value = as.numeric(x),
                     Method = factor(seq(1, length(as.numeric(x)))))
  ggplot(pdat, aes(y = Value,
                   x = Method,
                   group = 1)) +
    geom_line() + theme_spark()
}
