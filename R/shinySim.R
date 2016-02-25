#' Launch Shiny interface for MCSS results
#'
#' \code{shinySim}launches an interactive shiny interface
#' specifically designed for the analysis of simulation study results.
#'
#' @param x An \code{R} dataframe object, specifically of class \code{SimDesign}.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' data(tsimresults)
#' str(tsimresults)
#' head(tsimresults)
#' runApp(shinySim(tsimresults))
#' }
#'
#' @seealso \code{\link{SimDisplay}}
shinySim <- function(x){
  library(shiny)
  library(ggplot2)
  # Check datafile for simDat structure
  simDat <- sim_design_check(x)

  ret <- list(

    ui = pageWithSidebar(
      # Application title
      headerPanel("Simulation Display"),
      #-----------------------------------------------------------------------
      sidebarPanel(
        h5('Use the following to produce your display'),
        selectInput(inputId = "rowvar",
                           label = "Row variable:",
                           choices = get_design_levels(x),
                           selected = get_design_levels(x)[1]),
        selectInput(inputId = "colvar",
                           label = "Column Variable:",
                           choices = get_design_levels(x),
                           selected = get_design_levels(x)[2]),
        selectInput(inputId = "facetvar",
                           label = "Facet Table?",
                           choices = c("No" = "No",
                                       "Yes" = "Yes"),
                           selected = "No"),
        conditionalPanel(condition = 'input.facetvar == "Yes"',
                         selectInput(inputId = "facvar",
                                     label = "Facet Variable:",
                                     choices = get_design_levels(x),
                                     selected = get_design_levels(x)[3])),
        selectInput(inputId = "responsevar",
                           label = "Response Variable:",
                           choices = get_sim_levels(x),
                           selected = get_sim_levels(x)[1])
        ),
        #-----------------------------------------------------------------------
        mainPanel(
          plotOutput(outputId = "main_plot", height = "700px", width = "700px"),
          textOutput(outputId = "facet_status")
        )),
#-----------------------------------------------------------------------
    server = function(input, output) {
      getdat <- function(input){
        if(input$facetvar == "No") {
          dat <- simDat[,c(input$rowvar, input$colvar, input$responsevar)]
        }
        if(input$facetvar == "Yes") {
          dat <- simDat[,c(input$rowvar, input$colvar, input$responsevar, input$facvar)]
        }
        return(dat)
      }

      output$facet_status <- renderPrint(c(names(getdat(input)), input$facetvar, input$rowvar, input$colvar, input$responsevar))

      output$main_plot <- renderPlot({
          dat <- getdat(input)
          p <- ggplot(data = dat, aes_string(y=input$colvar, x=input$rowvar)) +
            geom_raster(aes_string(fill=input$responsevar)) +
            labs(x = input$colvar, y = input$rowvar)
          print(p)
          if (input$facetvar == "Yes") {
            p + facet_grid(paste(". ~ ", input$facvar))
          }
        })
    }
  )

  return(ret)
}
