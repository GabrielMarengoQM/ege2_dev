# main.R
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, 
        uiOutput, observe, reactiveVal, fluidRow, column, bookmarkButton,
        setBookmarkExclude, enableBookmarking, onBookmark, onRestore, req,
        navbarPage, tabPanel, navbarMenu],
  htmltools[p]
)

box::use(
  app/view/filters,
  app/view/edit_clear,
  app/view/go_enrichment
)

# enable Shiny bookmarking ----
enableBookmarking(store = 'server')

#' @export
ui <- function(id) {
  ns <- NS(id)
  # bootstrapPage(
  #   fluidRow(
  #     filters$ui(ns("filters"))
  #   ),
  #   fluidRow(
  #     column(1, p("Saved lists")),
  #     column(8, edit_clear$ui(ns("edit_clear"))),
  #     column(3, bookmarkButton())
  #   )
  # )
  navbarPage(
    'EGE',
    tabPanel(
      'Generate gene lists',
      fluidRow(
        filters$ui(ns("filters"))
      ),
      fluidRow(
        column(1, p("Saved lists")),
        column(8, edit_clear$ui(ns("edit_clear"))),
        column(3, bookmarkButton())
      )
    ),
    navbarMenu(
      'Analysis',
      tabPanel(
        'Gene Ontology Semantic Similarity',
        go_enrichment$ui(ns("go_enrichment"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    saved_lists_and_filters <- reactiveVal(list())
    edited_list <- reactiveVal(NULL)
    filters$server("filters", saved_lists_and_filters, edited_list)
    edit_clear$server("edit_clear", saved_lists_and_filters, edited_list)
    go_enrichment$server("go_enrichment", saved_lists_and_filters)
    
    # Handle bookmarking ----
    onBookmark(function(state) {
      # Save list name, list, filters
      state$values$saved_lists_and_filters <- saved_lists_and_filters()
    })
    
    onRestore(function(state) {
      if (!is.null(state$values$saved_lists_and_filters)) {
        saved_lists_and_filters(state$values$saved_lists_and_filters)
      }
    })
    
    observe({
      if (length(saved_lists_and_filters()) > 0) {
        print(length(unique(saved_lists_and_filters()[[1]][[1]])))
        # print(saved_lists_and_filters())
      }
    })
  })
}
