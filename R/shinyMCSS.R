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
#' @param percents \code{Boolean}, indicating whether to display numeric simulation
#' results as percentages.
#'
#' @param ndigits \code{Integer}. If \code{percents} is true, how many digits should be printed?
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


shinyMCSS <- function(dataframe = NULL, percents = FALSE, ndigits = 1, export = FALSE, browser = TRUE){
  library(shiny)
  library(shinydashboard)
  library(car)
  library(DT)
  options(DT.options = list(paging=FALSE,
                            dom = 'ltir'))

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

  dat <- data.frame(dat[dvars], dat[rvars], dat[mvars],
                    int7rn4l1d = dat$int7rn4l1d)

  ncases <- nrow(dat) # Used to track and report if filters are functioning across tabs

  ## UI #############################
  ui = navbarPage("Shiny SimDisplay",
    tabPanel("Data Explorer", value = "init", icon = icon("dashboard"),
             sidebarLayout(
               sidebarPanel(
                 h4("Filters:"),
                 checkboxGroupInput(inputId = "design",
                                    label = "Design Variables",
                                    choices = dvars,
                                    selected = dvars),
                 uiOutput("filters"),
                 checkboxGroupInput(inputId = "response",
                                    label = "Response Variables",
                                    choices = rvars,
                                    selected = rvars),
                 checkboxInput(inputId = "meta",
                               label = "Show meta variables?", value = FALSE),
                 checkboxInput(inputId = "percents",
                               label = "Show sim variables as percents?",
                               value = percents),
                 width = 2),
               mainPanel(
                 DT::dataTableOutput("data")
               ))
             ),
    tabPanel("Models", value = "models", icon = icon("table"),
             sidebarLayout(
               sidebarPanel(
                 h4("Model Options:"),
                 uiOutput("mod_des"),
                 uiOutput("mod_outcome"),
                 checkboxInput(inputId = "all_int",
                               label = "Add all interactions?", value = FALSE),
                 width = 2),
               mainPanel(
                 fluidRow(
                   h4("ANOVA Model Generator"),
                   box(width = 12,
                       verbatimTextOutput("models"))
                 )
               )
             )),
    tabPanel("Visualizations", value = "dataviz", icon = icon("line-chart"),
             sidebarLayout(
               sidebarPanel(
                 h4("Graphic Options:"),
                 selectInput(inputId = "graphic",
                             label = "Graphic Type:",
                             choices = c("Shaded Table" = "shade",
                                         "Boxplot" = "box",
                                         "HE Plot" = "he"),
                             multiple = FALSE,
                             selectize = FALSE),
                 conditionalPanel(condition = "input.graphic == 'shade'",
                                  h4("SHADED TABLE SELECTED")),
                 conditionalPanel(condition = "input.graphic == 'box'",
                                  h4("BOXPLOT SELECTED")),
                 conditionalPanel(condition="input.graphic == 'he'",
                                  h4("HE PLOT SELECTED")),
                 width = 2),
               mainPanel(
                 plotOutput("plot")
               ))
             ),
    inverse = TRUE, collapsible = TRUE)

  ## SERVER #########################
  server = function(input, output, session) {

    # REACTIVE DATAFRAME:
    dat_subset <- reactive({
      # SUBSET DATA COLUMNS BY DESIGN/META INPUTS
      if (input$meta){
        df <- dat[, c(input$design, input$response, mvars, "int7rn4l1d"), drop = FALSE]
        if (class(df$SIM_TIME) == "numeric"){
          df$SIM_TIME <- round(df$SIM_TIME, 2)
        }
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

      # SORT DATA BY INTERNALID (default)
      df <- df[order(df$int7rn4l1d),]
      return(df)
    })

    ## GENERATE UI ELEMENTS ###############

    ## DATA EXPLORER TAB ###########
    # DESIGN VARIABLE FILTER CHECKBOXES:
    output$filters <- renderUI({
      filters <- lapply(dvars[dvars %in% input$design], function(d) {
        list(inputId = d, label = d,
             choices = levels(dat[[d]]),
             selected = levels(dat[[d]]))
      })
      lapply(filters, do.call, what = checkboxGroupInput)
    })

    ## ANOVA TAB ##################
    # GENERATE MODEL DATA FILTERS:
    output$mod_des <- renderUI({
      checkboxGroupInput(inputId = "mod_filters",
                         label = "Design Variables",
                         choices = input$design,
                         selected = NULL)
    })

    # GENERATE MODEL OUTCOME SELECTOR:
    output$mod_outcome <- renderUI({
      selectizeInput(inputId = "mod_outcome",
                     label = "Outcome Variables",
                     choices = input$response,
                     selected = NULL,
                     multiple = TRUE)
    })

    ## VISUALIZATION TAB ##############

    ## RENDER OUTPUT ##################

    ## DATA EXPLORER TAB:
    output$data <- DT::renderDataTable({
      if (input$percents){
        DT::datatable(dat_subset()[!(colnames(dat_subset()) %in% c("int7rn4l1d"))],
                    rownames = FALSE,
                    selection = list(target = 'row+column'),
                    caption = 'Monte Carlo Simulation results datatable:',
                    options = list(saveState = TRUE)) %>%
        formatPercentage(input$response, digits = ndigits)
      } else {
        DT::datatable(dat_subset()[!(colnames(dat_subset()) %in% c("int7rn4l1d"))],
                      rownames = FALSE,
                      selection = list(target = 'row+column'),
                      caption = 'Monte Carlo Simulation results datatable:',
                      options = list(saveState = TRUE))
      }
    })

    ## ANOVA TAB:
    output$models <- renderPrint({
      if (is.null(input$mod_outcome) | is.null(input$mod_filters)){
        return(cat("Please select at least one design and one outcome variable.\n"))
      }

      # Generate univariate and multivariate formulas:
      if (length(input$mod_outcome) == 1){
        fm <- paste0(input$mod_outcome, "~",
                     paste(input$mod_filters, collapse = "+"))
      } else {
        fm <- paste0('cbind(',
                          paste(input$mod_outcome, collapse = ','),
                          ') ~ ',
                          paste(input$mod_filters, collapse = '+'))
      }

      # Incorporate interactions:
      if (length(input$mod_filters) > 1 & input$all_int == TRUE){
        fm <- gsub("\\+", "*", fm)
      }

      data <- dat_subset()
      mod <- do.call("lm",
                     list(formula = as.formula(fm),
                          data = as.name("data")))
    cat(paste0("Model formula: ", fm,
               "\nThis is based upon ", nrow(data),
               " of the ", ncases, " rows found in the original dataframe.\n\n"))
    print(Anova(mod))
    })

    ## VISUALIZATION TAB:
    output$plot <- renderPlot({
      type <- input$graphic

      if (type == "shade"){
        plot(x = rnorm(100), y = rnorm(100),
             main = "I'm a shaded table")
      }
      if (type == "box"){
        plot(x = rnorm(100), y = rnorm(100),
             main = "I'm a boxplot")
      }
      if (type == "he"){
        plot(x = rnorm(100), y = rnorm(100),
             main = "I'm a HE plot")
      }
    })

  }
  runApp(list(ui = ui, server = server), launch.browser = browser)
}
