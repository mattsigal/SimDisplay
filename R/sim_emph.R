#' Emphasize cells from simulation results
#'
#' \code{sim_emph} takes a \code{SimDesign} dataframe object, and returns a
#'
#' @param dat A \code{data.frame} object of \code{class(SimDesign)}.
#' @param upper.bound An \code{integer} value indicating the upper limit.
#' @param lower.bound An \code{integer} value indicating the lower limit.
#'
#' @return A character \code{vector}.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' Brown1974$REPLICATIONS <- Brown1974$SIM_TIME <- Brown1974$COMPLETED <- Brown1974$SEED <- NULL
#' TypeI <- subset(Brown1974, var_ratio == 1) # Separate type I error rate conditions
#' Power <- subset(Brown1974, var_ratio != 1) # from power conditions
#' cat(sim_emph(TypeI), sep = '\n')
#' }
#'
sim_emph <- function(dat, upper.bound = .075, lower.bound = .025){
  require(pander)
  panderOptions('table.split.table', Inf)
  panderOptions('table.style', 'simple')
  emph <- suppressWarnings(which(dat > upper.bound | dat < lower.bound, arr.ind = TRUE))
  ret <- pander_return(dat, round = 2, emphasize.strong.cells = emph)
  return(ret)
}