# gene_list_upload.r
box::use(
  shiny[moduleServer, NS, fileInput, observeEvent, observe, reactive,
        req, reactiveVal, uiOutput, renderUI, fluidRow, tableOutput,
        renderText, textOutput, renderTable, tagList, column, h4,
        actionButton],
  fst[read.fst],
  stats[setNames],
  purrr[map],
  shinyjs[reset, useShinyjs],
)

box::use(
  app/logic/file_upload_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    useShinyjs(),
    fileInput(
      ns("file_input"),
      "Upload gene list(s) as csv or tsv",
      multiple = TRUE,
      accept = NULL,
      width = "100%",
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    uiOutput(ns("gene_list_checker_ui")),
    actionButton(ns("save_upload"), "Save lists")
  )
}

#' @export
server <- function(id, saved_lists_and_filters, data) {
  moduleServer(id, function(input, output, session) {
    pcg_data <- read.fst("./data/pcg.checker.df.fst")
    vars <- reactive(names(data))
    file_data_reactive <- reactiveVal(NULL)

    observe({
      req(input$file_input)
      file_data <- file_upload_utils$userFilesUploadToList(input$file_input, pcg_data)
      lists <- file_data$genelist_names_list
      other_data <- file_data$other
      file_data_reactive(file_data)
    })

    output$gene_list_checker_ui <- renderUI({
      req(file_data_reactive())  # Ensure file_data_reactive is not NULL
      other_data <- file_data_reactive()$other_data
      summary_table <- fluidRow(tableOutput(session$ns("summary_table")))
      header <- "Gene checker"
      ui_elements <- lapply(names(other_data), function(name) {
        fluidRow(
          column(12,
                 h4(paste("Data for", name)),
                 tableOutput(outputId = session$ns(paste0(name, "_prev_tbl"))),
                 tableOutput(outputId = session$ns(paste0(name, "_alis_tbl"))),
                 textOutput(outputId = session$ns(paste0(name, "_no_match"))),
                 textOutput(outputId = session$ns(paste0(name, "_no_changes")))
          )
        )
      })

      # Return all UI elements for rendering
      tagList(summary_table, header, do.call(tagList, ui_elements))
    })

    output$summary_table <- renderTable({
      req(file_data_reactive())
      tbl <- data.frame(
        `Gene list` = names(file_data_reactive()$genelist_names_list),
        `List Length` =  sapply(file_data_reactive()$genelist_names_list, length)
      )
      colnames(tbl) <- c('Gene list', 'List Length')
      tbl
    })

    observe({
      req(file_data_reactive())
      other_data <- file_data_reactive()$other_data
      lapply(names(other_data), function(name) {
        output[[paste0(name, "_prev_tbl")]] <- renderTable({
          data <- other_data[[name]][[1]]
          req(nrow(data) > 0)
          data
        })
        output[[paste0(name, "_alis_tbl")]] <- renderTable({
          data <- other_data[[name]][[2]]
          req(nrow(data) > 0)
          data
        })
        output[[paste0(name, "_no_match")]] <- renderText({
          data <- other_data[[name]][[3]]
          req(length(data) > 0)
          paste("Unrecognised symbols: ", paste(data, collapse=", "))
        })
        output[[paste0(name, "_no_changes")]] <- renderText({
          req(nrow(other_data[[name]][[1]]) == 0)
          req(nrow(other_data[[name]][[2]]) == 0)
          req(length(other_data[[name]][[3]]) == 0)
          "No unrecognised, outdated or alias symbols detected"
        })
      })
    })

    observeEvent(input$save_upload, {
      req(file_data_reactive())
      current_lists <- saved_lists_and_filters()
      for (list_name in names(file_data_reactive()$genelist_names_list)) {
        current_filters <- map(vars(), function(var) {
          data_var <- data[[var]]
          if (is.numeric(data_var)) {
            # Reset numeric input: setting range to full data range
            val <- range(data_var, na.rm = TRUE)
          } else if (is.factor(data_var)) {
            # Reset select input: selecting all levels
            val <- levels(data_var)
          } else if (is.character(data_var)) {
            val <- ""
          }
          list(
            var = var,
            value = val,
            na_switch_value = TRUE
          )
        })
        # Assign current filters to the list using the list name as the key
        gene_list <- file_data_reactive()$genelist_names_list[[list_name]]
        current_filters[[1]]$value <- paste(gene_list, collapse = ';')
        saved_data <- list(gene_list, current_filters)
        current_lists[[list_name]] <- saved_data
      }
      saved_lists_and_filters(current_lists)
      shinyjs::reset("file_input")
    })
  })
}
