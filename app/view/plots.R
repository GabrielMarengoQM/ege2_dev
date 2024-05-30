box::use(
  shiny[moduleServer, NS, fluidRow, req, uiOutput, renderUI,
        observe, textAreaInput, reactiveVal, selectInput, updateSelectInput, reactive,
        checkboxInput],
  plotly[plotlyOutput, renderPlotly],
  purrr[map],
  bslib[navset_card_underline, nav_panel, card]
)

box::use(
  app/logic/plots_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    uiOutput(ns('plots'))
  )
}

#' @export
server <- function(id, saved_lists_and_filters, data) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    output$plots <- renderUI({
      req(length(saved_lists_and_filters()) > 0)
      navset_card_underline(
        nav_panel(
          "IMPC",
          selectInput(session$ns('gene_list_picker_mouse'), 'Select gene list',
                      choices = names(saved_lists_and_filters()),
                      selected = names(saved_lists_and_filters()),
                      multiple = TRUE),
          plots_utils$createUI(
            c("impc_viability", "wol", "impc_zygosity", "ortholog_mapping"),
            id = id,
            session = session
            )
          ),
        nav_panel(
          "Constraint metrics",
          selectInput(session$ns('gene_list_picker_constraint'), 'Select gene list',
                      choices = names(saved_lists_and_filters()),
                      selected = names(saved_lists_and_filters()),
                      multiple = TRUE),
          textAreaInput(session$ns('highlight_genes'), 'Enter genes to highlight'),
          checkboxInput(session$ns('toggle_points'), 'Show all data points', value = FALSE),
          plots_utils$createUI(
            c('gnomad_lof_upper_90_ci', 'mean_am_pathogenicity', 'shet_rgcme_mean', 'shet_post_mean', 'domino', 'scones',
              'mean_score_all', 'bf_lam', 'bf_mef'),
            id = id,
            session = session
          )
        )
      )
    })

    threshold_values <- list(
      NA, NA, NA, NA,
      0.35,    # for "gnomad_lof_upper_90_ci"
      0.9,     # for "mean_am_pathogenicity"
      0.075,   # for "shet_rgcme_mean"
      0.1,     # for "shet_post_mean"
      NULL,    # for "domino", since NULL is explicitly mentioned
      c(0.25, 0.75), # for "scones", this plot uses a vector of two thresholds
      c(-0.5, -1),   # for "mean_score_all", this plot also uses a vector of two negative thresholds
      5,       # for "bf_lam"
      5        # for "bf_mef"
    )
    axis_titles <- list(
      "IMPC viability", "Window of Lethality", "IMPC zygosity", "Ortholog mapping",
      "gnomAD LOEUF score", "Alpha Missense mean score", "Shet (Sun et al. 2023)", "Shet (Zeng et al. 2023)",
      "DOMINO score", "SCoNeS score", "DepMap mean gene effect score", "Bayes Factor (laminin grown hPSCs)", "Bayes Factor (MEF grown hPSCs)"
    )
    plot_ids <- c("impc_viability", "wol", "impc_zygosity", "ortholog_mapping", 'gnomad_lof_upper_90_ci',
                  'mean_am_pathogenicity', 'shet_rgcme_mean', 'shet_post_mean', 'domino', 'scones',
                  'mean_score_all', 'bf_lam', 'bf_mef')
    gene_lists_mouse <- reactive({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists <- map(saved_lists_and_filters(), ~ .x[['gene_list']])
      # Ensure names are preserved
      names(gene_lists) <- names(saved_lists_and_filters())
      gene_lists <- gene_lists[input$gene_list_picker_mouse]
    })
    highlighted_genes <- reactive({
      if (is.null(input$highlight_genes) || input$highlight_genes == "") {
        character(0)  # Return an empty character vector if no genes are entered
      } else {
        unlist(strsplit(input$highlight_genes, ";\\s*"))  # Split the input string into genes
      }
    })
    gene_lists_constraint <- reactive({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists <- map(saved_lists_and_filters(), ~ .x[['gene_list']])
      # Ensure names are preserved
      names(gene_lists) <- names(saved_lists_and_filters())
      gene_lists <- gene_lists[input$gene_list_picker_constraint]
    })
    toggle_points <- reactive({
      input$toggle_points
    })

    observe({
      req(length(saved_lists_and_filters()) > 0)

      highlighted_genes()
      gene_lists_constraint()
      toggle_points()
      plots_utils$plotServer(
        plot_ids = plot_ids,
        # plot_ids = names(data)[c(5:10, 20:30)],
        data = data,
        gene_lists = gene_lists_constraint(),
        bar_chart_func = plots_utils$barChart,
        violin_plot_func = plots_utils$violinPlot,
        axis_titles = axis_titles,
        genes_to_highlight = highlighted_genes(),
        threshold_values = threshold_values,
        toggle_option = toggle_points(),
        output = output
      )
    })


  })
}
