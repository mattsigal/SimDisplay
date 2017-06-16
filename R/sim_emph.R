#' Emphasize cells from simulation results
#'
#' \code{sim_emph} takes a \code{SimDesign} dataframe object, and returns a
#'
#' @param dat A \code{data.frame} object of \code{class(SimDesign)}.
#' @param upper.bound An \code{integer} value indicating the upper limit.
#' @param lower.bound An \code{integer} value indicating the lower limit.
#' @param colnames An optional \code{character} vector indicating column names to use in the output.
#' @param digits Display how many significant digits?
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
#' sim_emph(TypeI)
#' }
#'
sim_emph <- function(dat, upper.bound = .075, lower.bound = .025,
                     colnames = NULL, digits = 2){
  require(xtable)

  if (is.null(colnames)) {
    cn <- colnames(dat)
  } else cn <- colnames

  # Simple Dataframe:
  groupColumns <- SimDisplay:::get_design_levels(dat)
  dataColumns <- SimDisplay:::get_sim_levels(dat)
  df <- data.frame(dat[,c(groupColumns, dataColumns)])

  # Underscores cause problems for LaTeX:
  colnames(df) <- sub("_", "", cn)

  # Get cells to emphasize:
  emph <- suppressWarnings(which(df > upper.bound | df < lower.bound, arr.ind = TRUE))

  # Convert variables to character:
  i <- sapply(df, is.numeric)
  df[i] <- lapply(df[i], round_sim, digits = digits)

  # Add highlighting:
  highlights <- paste0('\\textbf{', df[emph], '}')
  df[emph] <- highlights

  out <- xtable(df, label = NULL, caption = NULL)
  print(out, type='latex',
        sanitize.text.function=identity,
        include.rownames=FALSE)
}