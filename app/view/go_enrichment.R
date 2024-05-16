box::use(
  shiny[moduleServer, NS, sidebarLayout, sidebarPanel, mainPanel, selectInput,
        actionButton, numericInput, fluidRow, HTML, checkboxInput, tableOutput,
        observeEvent, reactiveVal, req, observe, textOutput, renderPlot,
        renderText, onStop, invalidateLater, plotOutput, isolate, uiOutput,
        renderUI, tagList, updateSelectInput, reactiveValues],
  plotly[plotlyOutput, renderPlotly, ggplotly],
  ggplot2[...],
  fst[read.fst],
  clusterProfiler[...],
  rrvgo[...],
  stats[...],
  callr[...],
  org.Hs.eg.db[...],
  crew[...]
)

box::use(
  app/logic/enrichment_utils
)

# NEED TO ADD ERROR HANDLER

#' @export
ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("gene_list_picker"), "select gene list",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      selectInput(ns("ontology_picker"), "select GO ontology",
                  choices = c("BP", "MF", "CC")),
      numericInput(ns("slice_enriched_terms"), "Top n% enriched terms to retain", 25),
      numericInput(ns("p_val_input"), "p value cutoff", 0.01),
      numericInput(ns("q_val_input"), "q value cutoff", 0.05),
      numericInput(ns("similarity_score"),
                   HTML("Similarity score:<br> 0.9 (large), 0.7 (medium), 0.5 (small), 0.4 (tiny)"),
                   0.7),
      checkboxInput(ns("show_legend"), "Show legend"),
      actionButton(ns("task"), "Analyse")
    ),
    mainPanel(
      textOutput(ns('status')),
      uiOutput(ns("plots"))
      # tableOutput(ns("enriched_terms_table")),
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

    run_task <- enrichment_utils$goAnalysis

    # reactive values
    reactive_results <- reactiveValues()
    reactive_status <- reactiveVal("No task submitted yet")
    reactive_poll <- reactiveVal(FALSE)

    # outputs
    output$status <- renderText({reactive_status()})

    observe({

      lapply(names(reactive_results), function(task_name) {
        # unlike lists and envs, you can't remove values from reactiveValues, so we need this extra check
        # to make sure that we only get the plots that we asked for if we click the action
        # button multiple times after each other with different inputs
        if (task_name %in% isolate(input$gene_list_picker)) {
          output[[task_name]] <- renderPlotly(reactive_results[[task_name]])
        }
      })

    })

    output$plots <- renderUI({
      ns <- NS(id)
      req(reactive_poll() == FALSE)

      # create a list that holds all the plot outputs
      plot_output_list <- lapply(names(reactive_results), function(task_name) {
        if (task_name %in% isolate(input$gene_list_picker)) {
          plotlyOutput(session$ns(task_name))
        }
      })

      # create a list of tags
      tagList(plot_output_list)
    })

    # crew controller
    controller <- crew_controller_local(workers = 4, seconds_idle = 10)
    controller$start()

    # make sure to terminate the controller on stop
    onStop(function() controller$terminate())

    # button to submit a task
    observeEvent(input$task, {

      # create arguments list dynamically
      for (i in 1:length(input$gene_list_picker)) {

        symbol <- input$gene_list_picker[i]

        print(symbol)
        print(saved_lists_and_filters()[[symbol]][[1]])
        controller$push(
          command = run_task(gene_list, background, ontology, pval, qval, percent_slice, threshold),
          # pass the function to the workers, and arguments needed
          data = list(run_task = run_task,
                      gene_list = saved_lists_and_filters()[[symbol]][[1]],
                      background = unique(data$gene_symbol),
                      ontology = input$ontology_picker,
                      pval = input$p_val_input,
                      qval = input$q_val_input,
                      percent_slice = input$slice_enriched_terms,
                      threshold = input$similarity_score),
          name = symbol,
          packages = c("stats", "clusterProfiler", "rrvgo", "ggplot2", "org.Hs.eg.db")
        )
      }

      reactive_poll(TRUE)

    })

    # event loop to collect finished tasks
    observe({
      req(reactive_poll())
      invalidateLater(millis = 500)
      result <- controller$pop()

      if (!is.null(result)) {

        reactive_results[[result$name]] <- result$result[[1]]
      }
      reactive_poll(controller$nonempty())
    })

    observe({
      if(isTRUE(reactive_poll())) {
        reactive_status('Analysis running')
      } else if (isFALSE(reactive_poll())) {
        reactive_status('No task submitted yet')
      }
    })
  })
}



