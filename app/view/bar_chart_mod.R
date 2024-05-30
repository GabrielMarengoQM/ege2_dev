# bar_chart_mod.R
box::use(
  shiny[moduleServer, NS, uiOutput, renderUI, selectInput, reactive,
        observe, req, fluidRow],
  purrr[map]
)

box::use(
  app/logic/plots_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    uiOutput(ns('plots'))
  )
}

#' @export
server <- function(id, saved_lists_and_filters, data, plot_ids, footers, titles) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    output$plots <- renderUI({
      req(length(saved_lists_and_filters()) > 0)
      plots_utils$barChartUI(
        plot_ids = plot_ids,
        footers = footers,
        titles = titles,
        select_args = saved_lists_and_filters(),
        id = id,
        session = session
      )
    })

    gene_lists <- reactive({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists <- map(saved_lists_and_filters(), ~ .x[['gene_list']])
      names(gene_lists) <- names(saved_lists_and_filters())
      gene_lists <- gene_lists[input$gene_list_picker]
    })

    observe({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists()
      plots_utils$barChartServer(
        plot_ids = plot_ids,
        data = data,
        gene_lists = gene_lists(),
        plot_func = plots_utils$barChart,
        output = output
      )
    })
  })
}
