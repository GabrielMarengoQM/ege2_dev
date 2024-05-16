box::use(
  shiny[moduleServer, NS, fluidRow, req, uiOutput, renderUI, observe],
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
    # plotlyOutput(ns("plot"))
    uiOutput(ns('plots'))
  )
}

#' @export
server <- function(id, saved_lists_and_filters, data) {
  moduleServer(id, function(input, output, session) {
    # output$plot <- renderPlotly({
    #   req(length(saved_lists_and_filters()) > 0)
    #   # Extracting the first element from each list and preserving names
    #   gene_lists <- map(saved_lists_and_filters(), ~ .x[[1]])
    #   # Ensure names are preserved
    #   names(gene_lists) <- names(saved_lists_and_filters())
    #
    #   plots_utils$barChart(
    #     gene_list = gene_lists,
    #     data_source = data,
    #     data_col = 'impc_viability',
    #     custom_x_axis_title = 'impc_viability'
    #   )
    # })

    output$plots <- renderUI({
      req(length(saved_lists_and_filters()) > 0)
      navset_card_underline(
        nav_panel(
          "IMPC",
          plots_utils$createUI(
            c("impc_viability", "wol", "impc_zygosity", "ortholog_mapping"),
            id = id,
            session = session
            )
          ),
        nav_panel(
          "Constraint metrics",
          plots_utils$createUI(
            c('gnomad_lof_upper_90_ci', 'mean_am_pathogenicity', 'shet_rgcme_mean', 'shet_post_mean', 'domino', 'scones',
              'mean_score_all', 'bf_lam', 'bf_mef'),
            id = id,
            session = session
          )
        )
      )
    })

    observe({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists <- map(saved_lists_and_filters(), ~ .x[[1]])
      # Ensure names are preserved
      names(gene_lists) <- names(saved_lists_and_filters())
      plots_utils$plotServer(
        plot_ids = c("impc_viability", "wol", "impc_zygosity", "ortholog_mapping", 'gnomad_lof_upper_90_ci',
                     'mean_am_pathogenicity', 'shet_rgcme_mean', 'shet_post_mean', 'domino', 'scones',
                     'mean_score_all', 'bf_lam', 'bf_mef'),
        data = data,
        gene_lists = gene_lists,
        bar_chart_func = plots_utils$barChart,
        violin_plot_func = plots_utils$violinPlot,
        genes_to_highlight = NULL,
        threshold_value = 0.1,
        toggle_option = FALSE,
        output = output
      )
    })
  })
}
