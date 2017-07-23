#' Generate a tableplot.
#'
#' \code{simTableplot} takes a \code{SimDesign} dataframe object, and returns a tableplot.
#'
#' @param dat A \code{data.frame} object of \code{class(SimDesign)}.
#' @param design_vars An optional \code{character} vector pertaining to the design variables used to label each row. If omitted, all design variables will be used based upon the design levels attribute of the SimDesign object.
#' @param main_title A \code{character} value for the main title of the plot.
#'
#' @return NULL
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' TypeI <- subset(Brown1974, var_ratio == 1)
#' simTableplot(TypeI)
#' simTableplot(TypeI,
#'              design_vars = c("distribution", "sample_size"),
#'              main_title = "Type I error rates by distribution and sample size.")
#' }
#'

simTableplot <- function(dat,
                         design_vars = NULL,
                         main_title = "Tableplot"){
  # LIBRARIES
  require(tableplot)

  # SIMPLIFY DATAFRAME OBJECT
  if (is.null(design_vars)){
    groupColumns <- get_design_levels(dat)
  } else groupColumns <- design_vars
  dataColumns <- get_sim_levels(dat)

  df <- data.frame(dat[,c(groupColumns, dataColumns)])

  for (i in 1:nrow(df)){
    df$condition[i] <- as.character(interaction(df[i, groupColumns], sep=  "-", drop = TRUE))
  }

  # Simplify the dataframe and provide meaningful rownames:
  tdat <- as.matrix(round(df[,dataColumns], 2))
  rownames(tdat) <- df$condition

  # Assign the tableplot aesthetics:
  specs <- make.specs(
    shape = c(0),
    cell.fill=c('grey40'),
    back.fill="white",
    scale.max=1,
    label=1)

  # GENERATE PLOT
  tableplot(tdat, cell.specs = specs,
            left.space = max(nchar(rownames(tdat)))*2,
            title = main_title)
}
