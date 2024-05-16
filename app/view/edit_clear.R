# edit_clear.R
box::use(
  shiny[moduleServer, NS, fluidRow, uiOutput, actionButton, renderUI,
        req, column, HTML, tagList, observe, observeEvent,
        updateActionButton, reactive]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    uiOutput(ns("edit_clear_lists"))
  )
}

#' @export
server <- function(id, saved_lists_and_filters, edited_list) {
  moduleServer(id, function(input, output, session) {
    # render UI elements ----
    output$edit_clear_lists <- renderUI({
      names_list <- names(saved_lists_and_filters())
      req(length(names_list) > 0)
      edit_clear_lists <- lapply(names_list, function(name) {
        fluidRow(
          column(2, HTML(paste(name, ":"))),
          column(2, actionButton(session$ns(paste0("edit_list_", name)), label = 'Edit')),
          column(2, actionButton(session$ns(paste0("clear_list_", name)), label = "Clear"))
        )
      })
      do.call(tagList, edit_clear_lists)
    })

    # Delete/Clear list ----
    observe({
      names_list <- names(saved_lists_and_filters()) # Access within observe ensures it's in a reactive context
      lapply(names_list, function(name) {
        observeEvent(input[[paste0("clear_list_", name)]], {
          # Update the reactive value by removing the list associated with the button
          saved_lists <- saved_lists_and_filters()
          saved_lists[[name]] <- NULL  # Remove the list
          saved_lists_and_filters(saved_lists)  # Update the reactive variable
          edited_list(NULL)
        }, ignoreInit = TRUE)
      })
    })

    # Edit list ----
    observe({
      names_list <- names(saved_lists_and_filters())
      lapply(names_list, function(name) {
        # Create an observeEvent for each action button associated with each list
        observeEvent(input[[paste0("edit_list_", name)]], {
          # Update the edited list reactive value with filters for currently edited list
          filters <- saved_lists_and_filters()[[name]][[2]]
          data_to_add <- list('name' = name, 'filters' = filters)
          edited_list(data_to_add)
        }, ignoreInit = TRUE)
      })
    })
  })
}
