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
  library(shinydashboard)

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
  ui = dashboardPage(
    dashboardHeader(title = "Shiny SimDisplay"),
    skin = "black",

    # DASHBOARD SIDEBAR
    dashboardSidebar(
      sidebarMenu(
        menuItem("Initialization", tabName = "init", icon = icon("dashboard")),
        menuItem("Models", tabName = "models", icon = icon("table")),
        menuItem("Visualizations", tabName = "dataviz", icon = icon("line-chart"))
      )
    ),

    # DASHBOARD BODY
    dashboardBody(
      tabItems(
        # INITIALIZATION TAB
        tabItem(tabName = "init",
                fluidRow(
                  # Checkbox inputs for design, response, and meta variables:
                  box(title="Design Variables and Filters",
                      width = 4, solidheader = TRUE, status = "primary",
                      checkboxGroupInput(inputId = "design",
                                     label = NULL,
                                     choices = attributes(dat)$design_names$design,
                                     selected = attributes(dat)$design_names$design),
                      uiOutput("filters")),
                  box(title = "Response Variables:",
                      width = 4, solidheader = TRUE, status = "primary",
                      checkboxGroupInput(inputId = "response",
                                         label = NULL,
                                         choices = attributes(dat)$design_names$sim,
                                     selected = attributes(dat)$design_names$sim)),
                  box(title = "Meta Variables:",
                      width = 4, solidheader = TRUE, status = "warning",
                      checkboxGroupInput(inputId = "meta",
                                     label = NULL,
                                     choices = attributes(dat)$design_names$extra,
                                     selected = NULL)),
                  dataTableOutput("data")
                )),
        # MODELS TAB
        tabItem(tabName = "models",
                h2("Models")),
        # DATA VIZ TAB
        tabItem(tabName = "dataviz",
                h2("Data Visualizations"))
      ) # end of tabitems
    ) # end of dashboard body
  ) # end of dashboard page

  ## SERVER #########################
  server = function(input, output, session) {

    # SUBSET COLUMNS BASED UPON INPUTS:
    dat_subset <- reactive({
      cols <- subset(dat,
                     select = c(input$design, input$response, input$meta),
                     drop = FALSE)
      # THIS SHOULD ALSO UTILIZE THE ROW FILTERS THAT ARE GENERATED BELOW!!!

      for (i in 1:length(input$design)){
        var <- input$design[[i]]
        if (!is.null(input[[var]])){
          print(input[[var]]) # returns the levels of the design factor
          keep_rows <- input[[var]] %in% levels(dat[[var]])

        }
      }

      return(cols)
    })

    # RENDER FILTERS FOR DESIGN VARIABLES:
    first_run <<- TRUE # FLAG FOR SCRIPT INITIALIZATION
    output$filters <- renderUI({
      if (first_run){
        num_filters <- isolate(length(input$design))
        filters <- list()
        for (i in 1:num_filters){
          filt <- input$design[[i]]
          filters[[i]] <- checkboxGroupInput(inputId = filt,
                                             label = filt,
                                             choices = levels(dat_subset()[[filt]]),
                                             selected = levels(dat_subset()[[filt]]))
        }
        filters_save <<- filters
        first_run <<- FALSE
        filters
      } else filters_save
    })

    # RENDER DATA TABLE:
    output$data <- renderDataTable({
      dat_subset()
    }, options = list(paging = FALSE,
                      dom  = '<"top">lrt<"bottom">ip'))
  }

  # RUN THE SHINY APP:
  runApp(list(ui = ui, server = server))
}

#shinyMCSS()
