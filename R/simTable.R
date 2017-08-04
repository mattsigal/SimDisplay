#' Emphasize cells from simulation results
#'
#' \code{simTable} takes a \code{SimDesign} dataframe object, and returns a LaTeX table.
#'
#' @param dat A \code{data.frame} object of \code{class(SimDesign)}.
#' @param by A \code{character} value indicating a factor variable to collapse results by.
#' @param highlight A \code{boolean} value indicating if values should be highlighted based upon \code{upper.bound} and \code{lower.bound}.
#' @param upper.bound An \code{integer} value indicating the upper limit.
#' @param lower.bound An \code{integer} value indicating the lower limit.
#' @param colnames An optional \code{character} vector indicating column names to use in the output.
#' @param lucid Should results be printed? This converts results to characters of same length.
#' @param digits Display lucid results to how many significant digits?
#' @param caption A \code{character} string indicating caption.
#' @param rm A \code{character} vector indicating the names of any columns to hide from the table.
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
#' simTable(TypeI, by = 'sample_size')
#' simTable(TypeI, by = 'sample_size', rm = 'var_ratio')
#' }
#'
simTable <- function(dat, by = NULL,
                     highlight = TRUE,
                     upper.bound = .075, lower.bound = .025,
                     colnames = NULL, lucid = TRUE, digits = 2,
                     caption = NULL, rm = NULL){

  # Simple Dataframe:
  groupColumns <- get_design_levels(dat)
  dataColumns <- get_sim_levels(dat)
  df <- data.frame(dat[,c(groupColumns, dataColumns)]) # simplify dataframe object

  df <- df[ , !(names(df) %in% rm), drop = FALSE] # remove unwanted columns
  groupColumns <- groupColumns[!(groupColumns %in% rm)]
  dataColumns <- dataColumns[!(dataColumns %in% rm)]

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

  colnames(df) <- gsub(pattern = "_", replacement=paste("\\\\", "_", sep=""), cn)

  # Get cells to emphasize:
  emph <- suppressWarnings(which(df > upper.bound | df < lower.bound, arr.ind = TRUE))

  # If lucid, convert variables to character:
  if (lucid) {
    df[dataColumns] <- lapply(df[dataColumns], roundSim, digits = digits)
  }

  # Add highlighting:
  if (highlight) {
    highlights <- paste0('\\textbf{', df[emph], '}')
    df[emph] <- highlights
  }

  # Left align factors, right align numeric output:
  if (is.null(by)){
    alignment <- c("c", rep("l", length(groupColumns)), rep("r", length(dataColumns)))
  } else alignment <- c("c", rep("l", length(groupColumns)-1), rep("r", length(dataColumns)))

  # Generate table:
  out <- xtable::xtable(df, label = NULL, caption = caption, align = alignment)

  print(out, type='latex',
        sanitize.text.function=identity,
        include.rownames=FALSE, table.placement="H",
        caption.placement = "top")
}