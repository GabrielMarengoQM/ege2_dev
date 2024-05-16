box::use(
  shiny[moduleServer, NS, sidebarLayout, sidebarPanel, mainPanel, selectInput,
        actionButton, numericInput, fluidRow, HTML, checkboxInput, tableOutput,
        observeEvent, reactiveVal, req, observe, textOutput, renderPlot,
        renderText, onStop, invalidateLater, plotOutput, isolate],
  plotly[plotlyOutput, renderPlotly, ggplotly],
  ggplot2[...],
  fst[read.fst],
  clusterProfiler[...],
  rrvgo[...],
  stats[...],
  callr[...],
  org.Hs.eg.db[...]
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
                  choices = NULL),
      selectInput(ns("ontology_picker"), "select GO ontology",
                  choices = c("BP", "MF", "CC")),
      numericInput(ns("slice_enriched_terms"), "Top n% enriched terms to retain", 25),
      numericInput(ns("p_val_input"), "p value cutoff", 0.01),
      numericInput(ns("q_val_input"), "q value cutoff", 0.05),
      numericInput(ns("similarity_score"),
                   HTML("Similarity score:<br> 0.9 (large), 0.7 (medium), 0.5 (small), 0.4 (tiny)"),
                   0.7),
      checkboxInput(ns("show_legend"), "Show legend"),
      actionButton(ns("run_analysis"), "Analyse")
    ),
    mainPanel(
      textOutput(ns('status')),
      plotlyOutput(ns("go_plot"))
      # tableOutput(ns("enriched_terms_table")),
    )
  )
}

#' @export
server <- function(id, saved_lists_and_filters, data) {
  moduleServer(id, function(input, output, session) {

    # CALLR WORKING ----
    run_task <- enrichment_utils$goAnalysis

    reactive_status <- reactiveVal('No task submitted yet')
    reactive_result <- reactiveVal(ggplot())
    reactive_poll <- reactiveVal(FALSE)
    bg_proc <- reactiveVal(NULL)
    output$go_plot <- renderPlotly(reactive_result())
    output$status <- renderText({reactive_status()})

    observeEvent(input$run_analysis, {
      reactive_status('Running')
      p <- callr::r_bg(
        func = function(run_task, gene_list, background, ontology, pval, qval, percent_slice, threshold) {
          library('stats')
          library('clusterProfiler')
          library('rrvgo')
          library('ggplot2')
          library('org.Hs.eg.db')

          return(run_task(gene_list, background, ontology, pval, qval, percent_slice, threshold))
        },
        supervise = TRUE,
        args = list(run_task = run_task,
                    gene_list = unique(saved_lists_and_filters()[[1]][[1]]),
                    background = unique(data$gene_symbol),
                    ontology = input$ontology_picker,
                    pval = input$p_val_input,
                    qval = input$q_val_input,
                    percent_slice = input$slice_enriched_terms,
                    threshold = input$similarity_score)
      )

      bg_proc(p)
      reactive_poll(TRUE)
    })

    observe({
      req(reactive_poll())
      invalidateLater(100)
      p <- isolate(bg_proc())
      if (p$is_alive() == FALSE) {
        reactive_poll(FALSE)
        bg_proc(NULL)

        reactive_result(p$get_result())

        reactive_status('Done')
      }
    })
  })
}
