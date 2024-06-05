box::use(
  shiny[moduleServer, NS, tableOutput, fluidRow, observe, renderTable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    tableOutput(ns("data"))
  )
}

#' @export
server <- function(id, current_genes) {
  moduleServer(id, function(input, output, session) {
 
    observe({
      output$data <- current_genes()
    })
    
    

  })
}
