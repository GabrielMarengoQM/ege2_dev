box::use(
  shiny[moduleServer, NS, sidebarLayout, sidebarPanel, mainPanel, selectInput,
        actionButton, numericInput, fluidRow, HTML, checkboxInput, tableOutput,
        observeEvent, reactiveVal, req, observe, textOutput, renderPlot,
        renderText, onStop, invalidateLater, plotOutput, isolate, uiOutput,
        renderUI, tagList, updateSelectInput, reactiveValues, column, renderTable],
  plotly[plotlyOutput, renderPlotly, ggplotly],
  ggplot2[...],
  fst[read.fst],
  stats[...],
  crew[...],
  ReactomePA[enrichPathway],
  enrichplot[pairwise_termsim, emapplot],
  dplyr[...],
  bslib[card, card_header, card_body],
  htmltools[p, h6],
  shinycssloaders[withSpinner],
  purrr[map],
  conflicted[conflict_prefer]
)
# conflict_prefer("plot", "ggplot2")
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
      numericInput(ns("p_val"), "p value for enrichment analysis", 0.05),
      numericInput(ns("num_shown_pathways"), "Number of pathways displayed", 10),
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
      updateSelectInput(session, 'gene_list_picker', choices = names(saved_lists_and_filters()), selected = names(saved_lists_and_filters()))
    })

    run_task <- enrichment_utils$reactomeAnalysis

    # reactive values
    reactive_results <- reactiveValues()
    reactive_status <- reactiveVal("No task submitted yet")
    reactive_poll <- reactiveVal(FALSE)

    # outputs
    output$status <- renderText({reactive_status()})

    observe({

      lapply(names(reactive_results), function(task_name) {
        if (task_name %in% isolate(input$gene_list_picker)) {
          plot_id <- paste(task_name, "plot", sep="_")
          table_id <- paste(task_name, "table", sep="_")
          output[[plot_id]] <- renderPlot(reactive_results[[task_name]][[1]])
          output[[table_id]] <- renderTable(reactive_results[[task_name]][[2]])
        }
      })

    })

    output$plots <- renderUI({
      ns <- NS(id)
      req(reactive_poll() == FALSE)

      # create a list that holds all the plot outputs
      plot_output_list <- fluidRow(
        map(names(reactive_results), function(task_name) {
          if (task_name %in% isolate(input$gene_list_picker)) {
            plot_id <- session$ns(paste(task_name, "plot", sep="_"))
            table_id <- session$ns(paste(task_name, "table", sep="_"))
            column(
              width = 12,
              card(
                full_screen = TRUE,
                card_header(h6(task_name)),
                card_body(
                  fluidRow(plotOutput(plot_id) %>% withSpinner(color="#0dc5c1"))),
                  fluidRow(tableOutput(table_id))
              )
            )
          }
        })
      )
      # # create a list of tags
      # tagList(plot_output_list)
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

        controller$push(
          command = run_task(gene_list, background, pval, no_pathways_shown),
          # pass the function to the workers, and arguments needed
          data = list(run_task = run_task,
                      gene_list = saved_lists_and_filters()[[symbol]][['gene_list']],
                      background = unique(data[,c('gene_symbol', 'entrez_id')]),
                      pval = input$p_val,
                      no_pathways_shown = input$num_shown_pathways),
          name = symbol,
          packages = c("ReactomePA", "enrichplot", "ggplot2", "dplyr")
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



