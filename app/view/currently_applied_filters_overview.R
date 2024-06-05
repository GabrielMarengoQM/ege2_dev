box::use(
  shiny[moduleServer, NS, fluidRow, uiOutput, req, tagList,
        renderUI, column, actionButton, observeEvent],
  purrr[discard],
  htmltools[p]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(uiOutput(ns("current_filters_display")))

}

#' @export
server <- function(id, filter_differences, vars, nice_col_names, clear_filters) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    output$current_filters_display <- renderUI({
      req(!is.null(filter_differences()))

      differences <- filter_differences() # Fetch the reactive differences
      valid_differences <- discard(differences, is.null) # Discard nulls (no changes)

      # Prepare the UI elements
      ui_elements <- list()

      if (length(valid_differences) > 0) {
        # ui_elements[[1]] <- p("Current filters:")
        # Append each changed filter as a new UI element
        filter_ui <- lapply(valid_differences, function(filter) {
          index_in_vars <- match(filter$var, vars())
          # Access the corresponding nice name using the index
          nice_name <- nice_col_names[index_in_vars]
          fluidRow(
            column(6, p(nice_name)),  # Display the nice name
            column(3, p("Value: ", toString(filter$value))),
            column(3, p("NAs included: ", filter$na_switch_value))
          )
        })
        ui_elements <- c(ui_elements, filter_ui)

        ui_elements <- c(
          ui_elements,
          list(
            fluidRow(
              column(12, actionButton(session$ns("clear_filters"), "Clear filters"))
            )
          )
        )
      } else {
        # Add a message when no filters have changed
        ui_elements[[1]] <- p("No filters are currently applied")
      }
      do.call(tagList, ui_elements) # Combine all UI elements into a tagList
    })

    observeEvent(input$clear_filters, {
      clear_filters('clear')
    })

  })
}
