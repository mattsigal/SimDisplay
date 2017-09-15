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

shinyMCSS <- function(dataframe = NULL, percents = FALSE, ndigits = 2){
  pcks <- c("shiny", "shinydashboard", "car", "ggplot2", "heplots",
            "candisc", "RColorBrewer", "DT")
  check <- unlist(lapply(pcks, requireNamespace, quietly=TRUE))
  reqs <- data.frame(cbind(pcks, present = as.logical(check)))
  missingpcks <- paste(reqs[reqs$present == FALSE, 1], collapse = ", ")

  if (sum(reqs$present == FALSE) > 0) {
    stop(paste0("Please install the following package(s) before running shinyMCSS(): ", missingpcks),
         call. = FALSE)
  }
  rm(pcks, check, reqs, missingpcks)

  suppressMessages(require(shiny, quietly=T, warn.conflicts=F))
  suppressMessages(require(shinydashboard, quietly=T, warn.conflicts=F))
  suppressMessages(require(car, quietly=T, warn.conflicts=F))
  suppressMessages(require(ggplot2, quietly=T, warn.conflicts=F))
  suppressMessages(require(heplots, quietly=T, warn.conflicts=F))
  suppressMessages(require(candisc, quietly=T, warn.conflicts=F))
  suppressMessages(require(RColorBrewer, quietly=T, warn.conflicts=F))
  suppressMessages(require(DT, quietly=T, warn.conflicts=F))
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
                 numericInput(inputId = "rounding",
                              label = "Round results to...",
                              value = ndigits,
                              min = 0, max = 3, step = 1),
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
                 checkboxInput(inputId = "rates",
                               label = "Use logits?", value = TRUE),
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
                                         "Tableplot" = "table",
                                         "Boxplot" = "box",
                                         "HE Framework" = "he"),
                             multiple = FALSE,
                             selectize = FALSE),
                 conditionalPanel(condition = "input.graphic == 'shade'",
                                  textInput("shadeT", "Main Title:",
                                            value = "Shaded Table"),
                                  uiOutput("shadeX"),
                                  uiOutput("shadeF"),
                                  uiOutput("xlabeller"),
                                  textInput("labelS_Y", "Y-axis label:",
                                            value = "variable"),
                                  sliderInput("shadecol", "Font color switch:",
                                              min = 0, max = 1, value = .6,
                                              step = .01, ticks = FALSE),
                                  sliderInput("shaderow", "Number of rows:",
                                              min = 1, max = 3, value = 2,
                                              step = 1, ticks = FALSE),
                                  selectInput("shadecols", "Palette:",
                                              choices = rownames(brewer.pal.info)[18:35],
                                              selected = "Blues",
                                              multiple = FALSE,
                                              selectize = FALSE)
                                  ),
                 conditionalPanel(condition = "input.graphic == 'table'",
                                  textInput("tabtitle", "Main Title:",
                                            value = "Tableplot"),
                                  uiOutput("tabdes")),
                 conditionalPanel(condition = "input.graphic == 'box'",
                                  uiOutput("boxdes"),
                                  textInput("bTitle", "Main Title:",
                                            value = "Boxplot"),
                                  uiOutput("boxxlabeller"),
                                  textInput("bYlab", "Y-axis label:",
                                            value = "value"),
                                  checkboxInput("efford", "Order by means?",
                                                value = FALSE)),
                 conditionalPanel(condition="input.graphic == 'he'",
                                  selectInput("candisc", "Method:",
                                              choices = c("HE Plot", "CDA Plot",
                                                          "CDA HE Plot"),
                                              selected = "HE Plot"),
                                  conditionalPanel(condition = "input.candisc == 'HE Plot'",
                                                   uiOutput("heOut1"),
                                                   uiOutput("heOut2")),
                                  conditionalPanel(condition = "input.candisc == 'CDA HE Plot'",
                                                   checkboxInput("cdafill", "Fill?", value = TRUE),
                                                   sliderInput("cdafillslide", "Transparency",
                                                               min = 0, max = 1, value = .1, step = .01))
                                 ),
                 width = 2),
               mainPanel(
                 fluidRow(
                   box(width = 12,
                       plotOutput("plot"))
                 )
               ))
             ),
    tabPanel("About", value = "about", icon = icon("info-circle"),
             fluidPage(
               box(width = 10, status = "success",
                   shiny::includeMarkdown(system.file("about.Rmd", package="SimDisplay")))
             )
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
      checkboxGroupInput(inputId = "mod_outcome",
                     label = "Outcome Variables",
                     choices = input$response,
                     selected = NULL)
    })

    ## VISUALIZATION TAB ##############
    output$shadeX <- renderUI({
      selectInput(inputId = "xaxis",
                  label = "Facet:",
                  choices = input$design,
                  selected = input$design[[1]],
                  multiple = FALSE)
    })

    output$xlabeller <- renderUI({
      textInput("labelS_X", "X-axis label:", value = input$design[[2]])
    })

    output$shadeF <- renderUI({
      selectInput(inputId = "facet",
                  label = "X-axis:",
                  choices = input$design,
                  selected = input$design[[2]],
                  multiple = FALSE)
    })

    output$tabdes <- renderUI({
      checkboxGroupInput(inputId = "tabdesvars",
                         label = "Design labels:",
                         choices = input$design,
                         selected = input$design)
    })

    output$boxdes <- renderUI({
      selectInput(inputId = "bFacets",
                  label = "Facet by:",
                  choices = input$design,
                  selected = input$design[[1]],
                  multiple = FALSE)
    })

    output$boxxlabeller <- renderUI({
      textInput("bXlab", "X-axis label:", value = "variable")
    })

    output$heOut1 <- renderUI({
      selectInput(inputId = "hevars1",
                  label = "X-axis variable",
                  choices = input$mod_outcome,
                  selected = input$mod_outcome[[1]],
                  multiple = FALSE)
    })

    output$heOut2 <- renderUI({
      selectInput(inputId = "hevars2",
                  label = "Y-axis variable",
                  choices = input$mod_outcome,
                  selected = input$mod_outcome[[2]],
                  multiple = FALSE)
    })


    ## RENDER OUTPUT ##################

    ## DATA EXPLORER TAB:
    output$data <- DT::renderDataTable({
      if (input$percents){
        DT::datatable(dat_subset()[!(colnames(dat_subset()) %in% c("int7rn4l1d"))],
                    rownames = FALSE,
                    selection = list(target = 'row+column'),
                    caption = 'Datatable of Monte Carlo Simulation results. Filters applied in this tab will affect models and visualizations.',
                    options = list(saveState = TRUE)) %>%
        formatPercentage(input$response, digits = (input$rounding-2))
      } else {
        DT::datatable(dat_subset()[!(colnames(dat_subset()) %in% c("int7rn4l1d"))],
                      rownames = FALSE,
                      selection = list(target = 'row+column'),
                      caption = 'Datatable of Monte Carlo Simulation results. Filters applied in this tab will affect models and visualizations.',
                      options = list(saveState = TRUE)) %>%
          formatRound(input$response, digits = input$rounding)
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
      if (input$rates) {
        data[,input$mod_outcome] <-
          suppressWarnings(sapply(data[,input$mod_outcome], function(x) qlogis(x)))
      }
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
      dat <- dat_subset()[!(colnames(dat_subset()) %in% c("int7rn4l1d"))]

      if (input$graphic == "shade"){
        if (is.null(input$xaxis) | is.null(input$facet)){
          return()
        }
        dat <- convertDf(dat)

        suppressWarnings(print(tableShade(dat,
                         table_vars = c(input$xaxis, input$facet),
                         colswitch = input$shadecol,
                         numrow = input$shaderow,
                         colour = input$shadecols,
                         main_title = input$shadeT,
                         xlab = input$labelS_X,
                         ylab = input$labelS_Y)))
      }
      if (input$graphic == "table"){
        dat <- convertDf(dat)
        simTableplot(dat,
                     design_vars = input$tabdesvars,
                     main_title = input$tabtitle)
      }
      if (input$graphic == "box"){
        if (is.null(input$bFacets)){
          return()
        }
        dat <- suppressMessages(melt(dat))

        if (input$efford){
          dat$variable <- with(dat,
                               reorder(x = variable,
                                       X = value,
                                       FUN = function(x) mean(x)))
        }

        bplot <- ggplot(dat,
                        aes(variable, value, fill = variable)) +
          geom_boxplot() +
          facet_wrap(input$bFacets) +
          ggtitle(input$bTitle) +
          theme(legend.position='none') +
          ylab(input$bYlab) + xlab(input$bXlab)
        print(bplot)
      }
      if (input$graphic == "he"){
        validate(need(!is.null(input$mod_outcome) | !is.null(input$mod_filters),
                 "Please select at least one design and two outcome variables in the Models tab."))
        validate(need(length(input$mod_outcome) >= 2,
                 "Please select at least one design and two outcome variables in the Models tab."))
        if (is.null(input$hevars1)){
          return()
        }

        fm <- paste0('cbind(',
                       paste(input$mod_outcome, collapse = ','),
                       ') ~ ',
                       paste(input$mod_filters, collapse = '+'))

        # Incorporate interactions:
        if (length(input$mod_filters) > 1 & input$all_int == TRUE){
          fm <- gsub("\\+", "*", fm)
        }

        data <- dat_subset()
        mod <- do.call("lm",
                       list(formula = as.formula(fm),
                            data = as.name("data")))

        if (input$candisc == "HE Plot"){
          heplot(mod, main = paste0("HE Plot for model:\n", fm),
                      variables = c(input$hevars1, input$hevars2))
        }
        if (input$candisc == "CDA Plot"){
          mod <- candisc(mod)
          plot(mod, main = paste0("CDA Plot for model:\n", fm))
        }
        if (input$candisc == "CDA HE Plot"){
          mod <- candisc(mod)
          heplot(mod, main = paste0("CDA HE Plot for model:\n", fm),
                 fill = input$cdafill,
                 fill.alpha = input$cdafillslide)
        }
      }
    },
    width = "auto",
    height = 900,
    res = 96)
  }

  runApp(list(ui = ui, server = server), launch.browser = TRUE)
}
