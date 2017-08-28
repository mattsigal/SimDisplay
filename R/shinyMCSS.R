#' Generate a basic interactive applet for a SimDesign dataset
#'
#' \code{shinyMCSS} generates an interactive applet for the presentation and
#' analysis of a \code{SimDesign} dataframe. The backend was created using
#' the \code{shiny} package. The default applet is relatively straightforward,
#' but allows for the exporting of the relavant server and ui files to allow
#' further customization.
#'
#' @param dataframe an optional \code{dataframe} object of class \code{SimDesign}.
#'   If omitted, the app will load \code{data(Brown1974)}.
#'
#' @param export \code{Boolean}, indicating whether to save the generated server
#' and ui files to the current working directory.
#'
#' @param browser \code{Boolean} that is passed on to \code{runApp()} indicating
#' whether the app should be run in an RStudio window or in the default web browser
#'
#' @seealso \code{\link{Brown1974}}
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \url{http://www.tandfonline.com/doi/full/10.1080/10691898.2016.1246953}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load default internal dataset:
#' shinyMCSS()
#'
#' # Example of passing a SimDesign dataset from the current global environment:
#' data(Brown1974)
#' shinyMCSS(Brown1974)
#' }

shinyMCSS <- function(dataframe = NULL, export = FALSE, browser = TRUE){
  library(shiny)
  library(shinydashboard)

  # Load data:
  # Load example dataset or passed dataframe:
  if (is.null(dataframe)) {
    .get.bf <- function(){
      .bf.env <- new.env()
      data(Brown1974, package = 'SimDisplay', envir = .bf.env)
      .bf.env$Brown1974
    }
    dat <- .get.bf()
  } else dat <- dataframe

  # Get design variables (factors) and response variables:
  dvars <- attributes(dat)$design_names$design
  rvars <- attributes(dat)$design_names$sim
  mvars <- attributes(dat)$design_names$extra
  dat$int7rn4l1d <- 1:nrow(dat)

  ## UI #############################
  ui = fluidPage(
    titlePanel("Dynamic filtering example"),
    sidebarPanel(
      checkboxGroupInput(inputId = "design", label = "Design Variables",
                         choices = dvars, selected = dvars),
      uiOutput("filters"),
      checkboxGroupInput(inputId = "response", label = "Response Variables",
                         choices = rvars, selected = rvars),
      checkboxInput(inputId = "meta", label = "Show meta variables?",
                    value = FALSE)),
    mainPanel(
      dataTableOutput("data"))
  )

  ## SERVER #########################
  server = function(input, output, session) {

    # Determine checkboxes:
    output$filters <- renderUI({
      filters <- lapply(dvars[dvars == input$design], function(d) {
        list(inputId = d, label = d,
             choices = levels(dat[[d]]),
             selected = levels(dat[[d]]))
      })
      lapply(filters, do.call, what = checkboxGroupInput)
    })

    # GENERATE REDUCED DATA TABLE:
    dat_subset <- reactive({
      # SUBSET DATA COLUMNS BY DESIGN/META INPUTS
      if (input$meta){
        df <- dat[, c(input$design, input$response, mvars, "int7rn4l1d"), drop = FALSE]
      } else df <- dat[, c(input$design, input$response, "int7rn4l1d"), drop = FALSE]

      # SUBSET DATA BY ROWS AND MERGE
      for (i in 1:length(input$design)){
        if(!is.null(input[[input$design[[i]]]])){
          dfs <- lapply(input$design, function(d) {
            df[df[[d]] %in% input[[d]],]
          })
          if (length(dfs) > 1){
            df <- Reduce(function(...) merge(..., all=FALSE), dfs)
          } else df <- dfs[[1]]
        }
      }
      return(df)
    })

    output$data <- renderDataTable({
      dat_subset()[!(colnames(dat_subset()) %in% c("int7rn4l1d"))]
    })
  }
  runApp(list(ui = ui, server = server), launch.browser = browser)
}

