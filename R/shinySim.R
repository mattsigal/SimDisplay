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
#' library(shiny)
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
        # checkboxInput(inputId = "label_text",
        #               label = "Label cells?",
        #               value = FALSE),
        selectInput(inputId = "responsevar",
                           label = "Response Variable:",
                           choices = get_sim_levels(x),
                           selected = get_sim_levels(x)[1])
        ),
        #-----------------------------------------------------------------------
        mainPanel(
          plotOutput(outputId = "main_plot", height = "700px", width = "700px")
          #textOutput(outputId = "facet_status")
        )),
#-----------------------------------------------------------------------
    server = function(input, output) {

      # output$facet_status <- renderPrint(c(input$facetvar, input$rowvar,
      #                                      input$colvar, input$responsevar))

      output$main_plot <- renderPlot({
          if (input$facetvar == "No") {
            simDat <- simDat[,c(input$rowvar, input$colvar, input$responsevar)]
            means <- aggregate(simDat[,input$responsevar] ~ simDat[,input$rowvar] +
                                 simDat[,input$colvar],
                               FUN = "mean")
            colnames(means) <- c(as.character(input$rowvar),
                                 as.character(input$colvar),
                                 as.character(input$responsevar))
            means[,ncol(means)] <- round(means[,ncol(means)], 3)

            p <- ggplot(data = means,
                        aes_string(y=input$rowvar, x=input$colvar, label=input$responsevar)) +
              geom_raster(aes_string(fill=input$responsevar),
                          interpolate = FALSE) +
              geom_text(colour = 'black',
                         size = 5) +
              scale_fill_gradient2(low ="blue",
                                   mid = "white",
                                   high = "red",
                                   midpoint = 0.05,
                                   limits=c(0, .2)) +
              labs(x = input$colvar, y = input$rowvar)
            print(p)
          }

          if (input$facetvar == "Yes") {
            p <- ggplot(data = simDat,
                        aes_string(y=input$rowvar, x=input$colvar, label=input$responsevar)) +
              geom_raster(aes_string(fill=input$responsevar),
                          interpolate = FALSE) +
              geom_text(colour = 'black',
                        size = 5) +
              scale_fill_gradient2(low ="blue",
                                   mid = "white",
                                   high = "red",
                                   midpoint = 0.05,
                                   limits=c(0, .2)) +
              labs(x = input$colvar, y = input$rowvar)
            print(p)
            p + facet_grid(paste(". ~ ", input$facvar))
          }
        })
    }
  )

  return(ret)
}
