# main.R
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags,
        uiOutput, observe, reactiveVal, fluidRow, column, bookmarkButton,
        setBookmarkExclude, enableBookmarking, onBookmark, onRestore, req,
        navbarPage, tabPanel, navbarMenu],
  htmltools[p],
  fst[read.fst]
)

box::use(
  app/view/filters,
  app/view/edit_clear,
  app/view/go_enrichment,
  app/view/reactome_enrichment,
  app/view/plots,
  app/view/odds_ratio
)

# enable Shiny bookmarking ----
enableBookmarking(store = 'server')

#' @export
ui <- function(id) {
  ns <- NS(id)
  # tabs ----
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
    tabPanel(
      'Plots',
      fluidRow(
        plots$ui(ns("plots"))
      )
    ),
    navbarMenu(
      'Analysis',
      tabPanel(
        'Gene Ontology Semantic Similarity',
        go_enrichment$ui(ns("go_enrichment"))
      ),
      tabPanel(
        'Reactome enrichment',
        reactome_enrichment$ui(ns("reactome_enrichment"))
      ),
      tabPanel(
        'Odds Ratio',
        odds_ratio$ui(ns("odds_ratio"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # import data ----
    data <- read.fst("./data/all_data_ege.fst")

    # reactive values ----
    saved_lists_and_filters <- reactiveVal(list())
    edited_list <- reactiveVal(NULL)

    # module servers ----
    # Generate gene lists
    filters$server("filters", saved_lists_and_filters, edited_list, data)
    edit_clear$server("edit_clear", saved_lists_and_filters, edited_list)
    # Plots
    plots$server("plots", saved_lists_and_filters, data)
    # Analysis
    go_enrichment$server("go_enrichment", saved_lists_and_filters, data)
    reactome_enrichment$server("reactome_enrichment", saved_lists_and_filters, data)
    odds_ratio$server("odds_ratio", saved_lists_and_filters, data)

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

    # Debugging print statements
    observe({
      if (length(saved_lists_and_filters()) > 0) {
        print(length(unique(saved_lists_and_filters()[[1]][[1]])))
        # print(saved_lists_and_filters())
      }
    })
  })
}
