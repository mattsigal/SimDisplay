#' Generate a shaded table.
#'
#' \code{tableShade} takes a \code{SimDesign} dataframe object, and returns a shaded table.
#'
#' @param dat A \code{data.frame} object of \code{class(SimDesign)}.
#' @param table_vars A \code{character} vector pertaining to the variables that will be presented on the display. The first element specifies the facetting variable, the second the X-axis.
#' @param method A \code{character} vector of length 1 pertaining to the method to be used to summarize results.
#' @param ndigit An \code{integer} indicating the number of decimal places to use in the labelling.
#' @param colswitch A \code{numeric} value indicating where to switch from black text to white.
#' @param numrow An \code{integer} value indicating the number of rows to use in the facet grid.
#' @param cols A \code{character} value indicating the palette to use.
#' @param main_title A \code{character} value for the main title of the plot.
#' @param xlab A \code{character} value for the x-axis label.
#' @param ylab A \code{character} value for the y-axis label.
#' @param ... Additional parameters passed onto \code{ggplot2}.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso \code{\link{SimDisplay}}
#' @export
#'
#' @examples
#' \dontrun{
#' data(Brown1974)
#' dat <- subset(Brown1974, var_ratio != 1)
#' tableShade(dat, table_vars = c("distribution", "var_ratio"))
#'
#' # Adding optional arguments:
#' tableShade(dat, table_vars = c("distribution", "var_ratio"),
#'            main_title = "Power Rates by Variance Ratio and Distribution",
#'            xlab = "Variance Ratio", ylab = "Outcome Variable",
#'            numrow = 1, ndigit = 1, colswitch = .7, colours = "Reds")
#' }
#'

tableShade <- function(dat,
                       table_vars = NULL,
                       method = "mean",
                       ndigit = 2, colswitch = .6,
                       numrow = 2,
                       colours = "Blues", main_title = "Shaded Table",
                       xlab = NULL, ylab = NULL, ...){
  # LIBRARIES
  require(reshape2)
  require(RColorBrewer)
  require(ggplot2)
  require(dplyr)

  # GENERATE PALETTE
  myPalette <- colorRampPalette(brewer.pal(9, colours))

  # SIMPLIFY DATAFRAME OBJECT
  groupColumns <- get_design_levels(dat)
  dataColumns <- get_sim_levels(dat)
  dat <- data.frame(dat[,c(groupColumns, dataColumns)])

  # AGGREGATE DATAFRAME AS DESIRED
  f <- paste(". ~ ", paste(table_vars, collapse = " + "))
  dat <- aggregate(as.formula(f), data = dat, FUN = method)

  # REMOVE UNWANTED DESIGN COLUMNS
  keeps <- c(table_vars, dataColumns)
  dat <- dat[, keeps]

  # CONVERT TO LONG FORM
  dat <- suppressMessages(melt(dat))

  # REORDER VARIABLE LEVELS FOR PLOTTING
  levels(dat$variable) <- rev(sort(levels(dat$variable)))

  # VARIABLE FOR LUCID LABELLING
  dat$rval <- roundSim(dat$value, ndigit)

  # GENERATE PLOT
  pdat <- ggplot(dat,
                 aes_(x = as.name(table_vars[2])), ...) +
                 aes(y = variable,
                     fill = value) +
    facet_wrap(as.character(table_vars[1]),
               nrow = numrow,
               scales='free') +
    geom_tile() +
    scale_fill_gradientn(colours = myPalette(100)) +
    coord_equal() +
    ggtitle(main_title) +
    geom_text(data = dat %>%
                group_by(as.character(table_vars[1])) %>%
                dplyr::filter(value < colswitch),
                   aes(label = rval), hjust = 'right', nudge_x = .4, size = 5, color = 'black') +
    geom_text(data = dat %>%
                group_by(as.character(table_vars[1])) %>%
                dplyr::filter(value >= colswitch),
              aes(label = rval), hjust = 'right', nudge_x = .4, size = 5, color = 'white')

  # ADD AXIS LABELS IF PROVIDED:
  if (!is.null(xlab)) {
    pdat <- pdat + xlab(xlab)
  }
  if (!is.null(ylab)) {
    pdat <- pdat + ylab(ylab)
  }

  return(pdat)
}
