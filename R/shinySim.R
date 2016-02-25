#' shinySim
#' \code{shinySim} is a function that launches an interactive shiny interface
#' specifically designed for the analysis of simulation study designs.
#'
#' @param x An \code{R} dataframe object, specifically of class \code{SimDesign}.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#' @seealso \code{\link{SimDisplay}}
shinySim <- function(x){

  library(ggplot2)

  # Convert datafile to simDat structure
  simDat <- sim_design_check(x)

  ret <- list(

    ui = shiny::pageWithSidebar(

      # Application title
      shiny::headerPanel("Simulation Display"),

      shiny::sidebarPanel(

        shiny::h5('Use the following to produce your display'),

        shiny::selectInput(inputId = "rowvar",
                           label = "Row variable:",
                           choices = get_design_levels(x),
                           selected = get_design_levels(x)[1]),

        shiny::selectInput(inputId = "colvar",
                           label = "Column Variable:",
                           choices = get_design_levels(x),
                           selected = get_design_levels(x)[2]),

        shiny::selectInput(inputId = "facetvar",
                           label = "Facet Table?",
                           choices = c(No = "No",
                                       Yes = "Yes"),
                           selected = "No"),

        shiny::conditionalPanel(condition = "facetvar == 'Yes'",
                                shiny::selectInput(inputId = "facvar",
                                                   label = "Facet Variable:",
                                                   choices = get_design_levels(x),
                                                   selected = get_design_levels(x)[3])),

        shiny::selectInput(inputId = "responsevar",
                           label = "Response Variable:",
                           choices = get_sim_levels(x),
                           selected = get_sim_levels(x)[1])
        ),
        #-----------------------------------------------------------------------
        shiny::mainPanel(
          shiny::plotOutput(outputId = "main_plot", height = "700px", width = "700px"),
          shiny::textOutput(outputId = "facet_status")
        )),

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

      output$facet_status <- shiny::renderPrint(input$facetvar)

      output$main_plot <- shiny::renderPlot({
        dat <- getdat(input)
        p <- ggplot(data = dat,
                    aes(y=dat[,1],
                        x=dat[,2])) +
          geom_raster(aes(fill=dat[,3])) +
          labs(x = input$colvar,
               y = input$rowvar)

        print(p)

        if (input$facetvar == "Yes") {
          p + facet_grid(paste(". ~ ", colnames(dat,4)))
        }
        })
    }
  )

  return(ret)
}
