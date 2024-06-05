box::use(
  shiny[moduleServer, NS, fluidRow, textInput, actionButton, updateTextInput,
        req, observeEvent, column]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6,
      textInput(ns("name_saved_list"), NULL, placeholder = 'My list name')
    ),
    column(
      width = 6,
      actionButton(ns("save_filters"), "Save")
    )
  )
}

#' @export
server <- function(id, save_filters) {
  moduleServer(id, function(input, output, session) {

    # save filters ----
    observeEvent(input$save_filters, {
      list_name <- input$name_saved_list
      req(list_name != "")
      save_filters(list_name)
    })


  })
}
