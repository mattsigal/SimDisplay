#' Emphasize cells from simulation results
#'
#' \code{simTable} takes a \code{SimDesign} dataframe object, and returns a
#'
#' @param dat A \code{data.frame} object of \code{class(SimDesign)}.
#' @param by A \code{character} value indicating a factor variable to collapse results by.
#' @param upper.bound An \code{integer} value indicating the upper limit.
#' @param lower.bound An \code{integer} value indicating the lower limit.
#' @param colnames An optional \code{character} vector indicating column names to use in the output.
#' @param lucid Should results be printed? This converts results to characters of same length.
#' @param digits Display lucid results to how many significant digits?
#' @param caption A \code{character} string indicating caption.
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
#' simTable(TypeI)
#' }
#'
simTable <- function(dat, by = NULL,
                     upper.bound = .075, lower.bound = .025,
                     colnames = NULL, digits = 2, caption = NULL){
  # Simple Dataframe:
  df <- simplifyDf(dat)

  # Collapse Dataframe if needed:
  if (!is.null(by)) {
    mlt <- reshape2::melt(df, id.vars = groupColumns)
    groups <- groupColumns[!groupColumns %in% by]
    fm <- formula(paste0(paste0(groups, collapse = " + "), "~ variable"))
    df <- reshape2::dcast(mlt, fm, fun.aggregate = mean)
  }

  # Apply custom column names and remove underscores (cause problems for LaTeX):
  if (is.null(colnames)) {
    cn <- colnames(df)
  } else cn <- colnames

  colnames(df) <- sub("_", ".", cn)

  # Get cells to emphasize:
  emph <- suppressWarnings(which(df > upper.bound | df < lower.bound, arr.ind = TRUE))

  # If lucid, convert variables to character:
  if (lucid) {
    i <- sapply(df, is.numeric)
    df[i] <- lapply(df[i], roundSim, digits = digits)
  }

  # Add highlighting:
  highlights <- paste0('\\textbf{', df[emph], '}')
  df[emph] <- highlights

  # Left align factors, right align numeric output:
  if (!is.null(by)) {
    alignment <- c("c", rep("l", length(groupColumns)-1), rep("r", length(dataColumns)))
  } else alignment <- c("c", rep("l", length(groupColumns)), rep("r", length(dataColumns)))

  # Generate table:
  out <- xtable::xtable(df, label = NULL, caption = caption, align = alignment)

  print(out, type='latex',
        sanitize.text.function=identity,
        include.rownames=FALSE, table.placement="H",
        caption.placement = "top")
}