#' Generate a basic interactive applet for a SimDesign dataset
#'
#' \code{shinyMCSS} generates an interactive applet for the presentation and
#' analysis of a \code{SimDesign} dataframe. The backend was created using
#' the \code{shiny} package. The default applet is relatively straightforward,
#' but allows for the exporting of the relavant server and ui files to allow
#' further customization.
#'
#' @param filename an optional name of a \code{dataframe} of class \code{SimDesign}.
#'   If omitted, the app will load \code{data(Brown1974)}.
#'
#' @param export \code{Boolean}, indicating whether to save the generated server
#' and ui files to the current working directory.
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

shinyMCSS <- function(filename = NULL, export = FALSE){
  library(shiny)

  # Load example dataset or passed dataframe:
  if (is.null(filename)) {
    .get.bf <- function(){
      .bf.env <- new.env()
      data(Brown1974, package = 'SimDisplay', envir = .bf.env)
      .bf.env$Brown1974
    }
    dat <- data <- .get.bf()
  } else dat <- filename

  ## UI #############################
  ui = fluidPage(
    checkboxGroupInput(inputId = "design",
                   label = "Design Variables:",
                   choices = attributes(dat)$design_names$design,
                   selected = attributes(dat)$design_names$design),
    checkboxGroupInput(inputId = "response",
                   label = "Response Variables:",
                   choices = attributes(dat)$design_names$sim,
                   selected = attributes(dat)$design_names$sim),
    checkboxGroupInput(inputId = "meta",
                       label = "Meta Variables:",
                       choices = attributes(dat)$design_names$extra,
                       selected = attributes(dat)$design_names$extra),
    tableOutput("data")
  )

  ## SERVER #########################
  server = function(input, output, session) {
    output$data <- renderTable({
      dat[, c(input$design, input$response, input$meta), drop = FALSE]
    }, rownames = TRUE)
  }

  runApp(list(ui = ui, server = server))
}

# shinyMCSS()