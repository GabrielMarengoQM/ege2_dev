box::use(
  shiny[moduleServer, NS, sidebarLayout, sidebarPanel, selectInput, actionButton,
        mainPanel, textOutput, uiOutput, plotOutput, renderPlot, reactiveVal,
        observe, req, updateSelectInput, observeEvent, tableOutput, renderTable,
        renderText]
)

box::use(
  app/logic/enrichment_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("gene_list_picker"), "select gene list",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      selectInput(ns("data_picker"), "select data",
                  choices = c("OMIM Disease genes", "IMPC Lethal genes", "DepMap Essential genes")),
      actionButton(ns("task"), "Analyse")
    ),
    mainPanel(
      textOutput(ns('status')),
      plotOutput(ns("plot")),
      tableOutput(ns("table"))
    )
  )
}

#' @export
server <- function(id, saved_lists_and_filters, data) {
  moduleServer(id, function(input, output, session) {

    observe({
      saved_lists_and_filters()
      updateSelectInput(session, 'gene_list_picker', choices = names(saved_lists_and_filters()), selected = NULL)
    })

    forest_plot <- reactiveVal()
    table_data <- reactiveVal()
    status <- reactiveVal('Select gene list(s) and run analysis')

    observeEvent(input$task, {
      req(length(saved_lists_and_filters()) > 0)
      dat <- enrichment_utils$batchOddsRatioPlots(
        compare_list_option = input$data_picker,
        dataset = data,
        list_of_lists = saved_lists_and_filters()[input$gene_list_picker],
        odds_ratio_func = enrichment_utils$oddsRatioPlot
      )

      forest_plot(enrichment_utils$forestPlot(dat))
      table_data(dat)
      status(NULL)
    })

    output$plot <- renderPlot({forest_plot()})
    output$table <- renderTable({table_data()})
    output$status <- renderText({status()})

  })
}
